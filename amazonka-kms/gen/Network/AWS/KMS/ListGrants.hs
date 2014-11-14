{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.KMS.ListGrants
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | List the grants for a specified key.
module Network.AWS.KMS.ListGrants
    (
    -- * Request
      ListGrants
    -- ** Request constructor
    , listGrants
    -- ** Request lenses
    , lgKeyId
    , lgLimit
    , lgMarker

    -- * Response
    , ListGrantsResponse
    -- ** Response constructor
    , listGrantsResponse
    -- ** Response lenses
    , lgrGrants
    , lgrNextMarker
    , lgrTruncated
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.KMS.Types

data ListGrants = ListGrants
    { _lgKeyId  :: Text
    , _lgLimit  :: Maybe Natural
    , _lgMarker :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListGrants' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgKeyId' @::@ 'Text'
--
-- * 'lgLimit' @::@ 'Maybe' 'Natural'
--
-- * 'lgMarker' @::@ 'Maybe' 'Text'
--
listGrants :: Text -- ^ 'lgKeyId'
           -> ListGrants
listGrants p1 = ListGrants
    { _lgKeyId  = p1
    , _lgLimit  = Nothing
    , _lgMarker = Nothing
    }

-- | Unique identifier of the key. This can be an ARN, an alias, or a globally
-- unique identifier.
lgKeyId :: Lens' ListGrants Text
lgKeyId = lens _lgKeyId (\s a -> s { _lgKeyId = a })

-- | Specify this parameter only when paginating results to indicate the
-- maximum number of grants you want listed in the response. If there are
-- additional grants beyond the maximum you specify, the Truncated response
-- element will be set to true.
lgLimit :: Lens' ListGrants (Maybe Natural)
lgLimit = lens _lgLimit (\s a -> s { _lgLimit = a })

-- | Use this parameter only when paginating results, and only in a subsequent
-- request after you've received a response where the results are truncated.
-- Set it to the value of the NextMarker in the response you just received.
lgMarker :: Lens' ListGrants (Maybe Text)
lgMarker = lens _lgMarker (\s a -> s { _lgMarker = a })

instance ToPath ListGrants where
    toPath = const "/"

instance ToQuery ListGrants where
    toQuery = const mempty

instance ToHeaders ListGrants

instance ToBody ListGrants where
    toBody = toBody . encode . _lgKeyId

data ListGrantsResponse = ListGrantsResponse
    { _lgrGrants     :: [GrantListEntry]
    , _lgrNextMarker :: Maybe Text
    , _lgrTruncated  :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | 'ListGrantsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgrGrants' @::@ ['GrantListEntry']
--
-- * 'lgrNextMarker' @::@ 'Maybe' 'Text'
--
-- * 'lgrTruncated' @::@ 'Maybe' 'Bool'
--
listGrantsResponse :: ListGrantsResponse
listGrantsResponse = ListGrantsResponse
    { _lgrGrants     = mempty
    , _lgrNextMarker = Nothing
    , _lgrTruncated  = Nothing
    }

-- | A list of grants.
lgrGrants :: Lens' ListGrantsResponse [GrantListEntry]
lgrGrants = lens _lgrGrants (\s a -> s { _lgrGrants = a })

-- | If Truncated is true, this value is present and contains the value to use
-- for the Marker request parameter in a subsequent pagination request.
lgrNextMarker :: Lens' ListGrantsResponse (Maybe Text)
lgrNextMarker = lens _lgrNextMarker (\s a -> s { _lgrNextMarker = a })

-- | A flag that indicates whether there are more items in the list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the Marker request parameter to retrieve more grants in the list.
lgrTruncated :: Lens' ListGrantsResponse (Maybe Bool)
lgrTruncated = lens _lgrTruncated (\s a -> s { _lgrTruncated = a })

instance AWSRequest ListGrants where
    type Sv ListGrants = KMS
    type Rs ListGrants = ListGrantsResponse

    request  = post
    response = jsonResponse $ \h o -> ListGrantsResponse
        <$> o .: "Grants"
        <*> o .: "NextMarker"
        <*> o .: "Truncated"
