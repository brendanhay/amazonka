{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CodeDeploy.ListApplications
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the applications registered within the AWS user account.
module Network.AWS.CodeDeploy.ListApplications
    (
    -- * Request
      ListApplications
    -- ** Request constructor
    , listApplications
    -- ** Request lenses
    , laNextToken

    -- * Response
    , ListApplicationsResponse
    -- ** Response constructor
    , listApplicationsResponse
    -- ** Response lenses
    , lar1Applications
    , lar1NextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

newtype ListApplications = ListApplications
    { _laNextToken :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'ListApplications' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'laNextToken' @::@ 'Maybe' 'Text'
--
listApplications :: ListApplications
listApplications = ListApplications
    { _laNextToken = Nothing
    }

-- | An identifier that was returned from the previous list applications call,
-- which can be used to return the next set of applications in the list.
laNextToken :: Lens' ListApplications (Maybe Text)
laNextToken = lens _laNextToken (\s a -> s { _laNextToken = a })

data ListApplicationsResponse = ListApplicationsResponse
    { _lar1Applications :: [Text]
    , _lar1NextToken    :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListApplicationsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lar1Applications' @::@ ['Text']
--
-- * 'lar1NextToken' @::@ 'Maybe' 'Text'
--
listApplicationsResponse :: ListApplicationsResponse
listApplicationsResponse = ListApplicationsResponse
    { _lar1Applications = mempty
    , _lar1NextToken    = Nothing
    }

-- | A list of application names.
lar1Applications :: Lens' ListApplicationsResponse [Text]
lar1Applications = lens _lar1Applications (\s a -> s { _lar1Applications = a })

-- | If the amount of information that is returned is significantly large, an
-- identifier will also be returned, which can be used in a subsequent list
-- applications call to return the next set of applications in the list.
lar1NextToken :: Lens' ListApplicationsResponse (Maybe Text)
lar1NextToken = lens _lar1NextToken (\s a -> s { _lar1NextToken = a })

instance AWSRequest ListApplications where
    type Sv ListApplications = CodeDeploy
    type Rs ListApplications = ListApplicationsResponse

    request  = post
    response = jsonResponse

instance FromJSON ListApplicationsResponse where
    parseJSON = genericParseJSON jsonOptions

instance ToPath ListApplications where
    toPath = const "/"

instance ToHeaders ListApplications

instance ToQuery ListApplications where
    toQuery = const mempty

instance ToJSON ListApplications where
    toJSON = genericToJSON jsonOptions
