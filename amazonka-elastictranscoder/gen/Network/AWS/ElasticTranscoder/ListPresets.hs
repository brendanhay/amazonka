{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.ListPresets
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ListPresets operation gets a list of the default presets included
-- with Elastic Transcoder and the presets that you\'ve added in an AWS
-- region.
--
-- /See:/ <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/ListPresets.html AWS API Reference> for ListPresets.
module Network.AWS.ElasticTranscoder.ListPresets
    (
    -- * Creating a Request
      ListPresets
    , listPresets
    -- * Request Lenses
    , lAscending
    , lPageToken

    -- * Destructuring the Response
    , ListPresetsResponse
    , listPresetsResponse
    -- * Response Lenses
    , lrsNextPageToken
    , lrsPresets
    , lrsStatus
    ) where

import           Network.AWS.ElasticTranscoder.Types
import           Network.AWS.ElasticTranscoder.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The @ListPresetsRequest@ structure.
--
-- /See:/ 'listPresets' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lAscending'
--
-- * 'lPageToken'
data ListPresets = ListPresets'
    { _lAscending :: !(Maybe Text)
    , _lPageToken :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListPresets' smart constructor.
listPresets :: ListPresets
listPresets =
    ListPresets'
    { _lAscending = Nothing
    , _lPageToken = Nothing
    }

-- | To list presets in chronological order by the date and time that they
-- were created, enter @true@. To list presets in reverse chronological
-- order, enter @false@.
lAscending :: Lens' ListPresets (Maybe Text)
lAscending = lens _lAscending (\ s a -> s{_lAscending = a});

-- | When Elastic Transcoder returns more than one page of results, use
-- @pageToken@ in subsequent @GET@ requests to get each successive page of
-- results.
lPageToken :: Lens' ListPresets (Maybe Text)
lPageToken = lens _lPageToken (\ s a -> s{_lPageToken = a});

instance AWSPager ListPresets where
        page rq rs
          | stop (rs ^. lrsNextPageToken) = Nothing
          | stop (rs ^. lrsPresets) = Nothing
          | otherwise =
            Just $ rq & lPageToken .~ rs ^. lrsNextPageToken

instance AWSRequest ListPresets where
        type Sv ListPresets = ElasticTranscoder
        type Rs ListPresets = ListPresetsResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 ListPresetsResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "Presets" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders ListPresets where
        toHeaders = const mempty

instance ToPath ListPresets where
        toPath = const "/2012-09-25/presets"

instance ToQuery ListPresets where
        toQuery ListPresets'{..}
          = mconcat
              ["Ascending" =: _lAscending,
               "PageToken" =: _lPageToken]

-- | The @ListPresetsResponse@ structure.
--
-- /See:/ 'listPresetsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrsNextPageToken'
--
-- * 'lrsPresets'
--
-- * 'lrsStatus'
data ListPresetsResponse = ListPresetsResponse'
    { _lrsNextPageToken :: !(Maybe Text)
    , _lrsPresets       :: !(Maybe [Preset])
    , _lrsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListPresetsResponse' smart constructor.
listPresetsResponse :: Int -> ListPresetsResponse
listPresetsResponse pStatus_ =
    ListPresetsResponse'
    { _lrsNextPageToken = Nothing
    , _lrsPresets = Nothing
    , _lrsStatus = pStatus_
    }

-- | A value that you use to access the second and subsequent pages of
-- results, if any. When the presets fit on one page or when you\'ve
-- reached the last page of results, the value of @NextPageToken@ is
-- @null@.
lrsNextPageToken :: Lens' ListPresetsResponse (Maybe Text)
lrsNextPageToken = lens _lrsNextPageToken (\ s a -> s{_lrsNextPageToken = a});

-- | An array of @Preset@ objects.
lrsPresets :: Lens' ListPresetsResponse [Preset]
lrsPresets = lens _lrsPresets (\ s a -> s{_lrsPresets = a}) . _Default . _Coerce;

-- | Undocumented member.
lrsStatus :: Lens' ListPresetsResponse Int
lrsStatus = lens _lrsStatus (\ s a -> s{_lrsStatus = a});
