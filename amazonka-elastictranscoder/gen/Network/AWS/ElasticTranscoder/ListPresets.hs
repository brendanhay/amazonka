{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.ListPresets
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ListPresets operation gets a list of the default presets included with
-- Elastic Transcoder and the presets that you've added in an AWS region.
--
-- <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/ListPresets.html>
module Network.AWS.ElasticTranscoder.ListPresets
    (
    -- * Request
      ListPresets
    -- ** Request constructor
    , listPresets
    -- ** Request lenses
    , lp1Ascending
    , lp1PageToken

    -- * Response
    , ListPresetsResponse
    -- ** Response constructor
    , listPresetsResponse
    -- ** Response lenses
    , lpr1NextPageToken
    , lpr1Presets
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.ElasticTranscoder.Types
import qualified GHC.Exts

data ListPresets = ListPresets
    { _lp1Ascending :: Maybe Text
    , _lp1PageToken :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'ListPresets' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lp1Ascending' @::@ 'Maybe' 'Text'
--
-- * 'lp1PageToken' @::@ 'Maybe' 'Text'
--
listPresets :: ListPresets
listPresets = ListPresets
    { _lp1Ascending = Nothing
    , _lp1PageToken = Nothing
    }

-- | To list presets in chronological order by the date and time that they
-- were created, enter @true@. To list presets in reverse chronological
-- order, enter @false@.
lp1Ascending :: Lens' ListPresets (Maybe Text)
lp1Ascending = lens _lp1Ascending (\s a -> s { _lp1Ascending = a })

-- | When Elastic Transcoder returns more than one page of results, use
-- @pageToken@ in subsequent @GET@ requests to get each successive page of
-- results.
lp1PageToken :: Lens' ListPresets (Maybe Text)
lp1PageToken = lens _lp1PageToken (\s a -> s { _lp1PageToken = a })

data ListPresetsResponse = ListPresetsResponse
    { _lpr1NextPageToken :: Maybe Text
    , _lpr1Presets       :: List "Presets" Preset
    } deriving (Eq, Show)

-- | 'ListPresetsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpr1NextPageToken' @::@ 'Maybe' 'Text'
--
-- * 'lpr1Presets' @::@ ['Preset']
--
listPresetsResponse :: ListPresetsResponse
listPresetsResponse = ListPresetsResponse
    { _lpr1Presets       = mempty
    , _lpr1NextPageToken = Nothing
    }

-- | A value that you use to access the second and subsequent pages of
-- results, if any. When the presets fit on one page or when you've reached
-- the last page of results, the value of @NextPageToken@ is @null@.
lpr1NextPageToken :: Lens' ListPresetsResponse (Maybe Text)
lpr1NextPageToken =
    lens _lpr1NextPageToken (\s a -> s { _lpr1NextPageToken = a })

-- | An array of @Preset@ objects.
lpr1Presets :: Lens' ListPresetsResponse [Preset]
lpr1Presets = lens _lpr1Presets (\s a -> s { _lpr1Presets = a }) . _List

instance ToPath ListPresets where
    toPath = const "/2012-09-25/presets"

instance ToQuery ListPresets where
    toQuery ListPresets{..} = mconcat
        [ "Ascending" =? _lp1Ascending
        , "PageToken" =? _lp1PageToken
        ]

instance ToHeaders ListPresets

instance ToJSON ListPresets where
    toJSON = const (toJSON Empty)

instance AWSRequest ListPresets where
    type Sv ListPresets = ElasticTranscoder
    type Rs ListPresets = ListPresetsResponse

    request  = get
    response = jsonResponse

instance FromJSON ListPresetsResponse where
    parseJSON = withObject "ListPresetsResponse" $ \o -> ListPresetsResponse
        <$> o .:? "NextPageToken"
        <*> o .:  "Presets"

instance AWSPager ListPresets where
    page rq rs
        | stop (rq ^. lp1PageToken) = Nothing
        | otherwise = (\x -> rq & lp1PageToken ?~ x)
            <$> (rs ^. lpr1NextPageToken)
