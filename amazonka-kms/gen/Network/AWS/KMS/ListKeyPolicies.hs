{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.KMS.ListKeyPolicies
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves a list of policies attached to a key.
module Network.AWS.KMS.ListKeyPolicies
    (
    -- * Request
      ListKeyPolicies
    -- ** Request constructor
    , listKeyPolicies
    -- ** Request lenses
    , lkpKeyId
    , lkpLimit
    , lkpMarker

    -- * Response
    , ListKeyPoliciesResponse
    -- ** Response constructor
    , listKeyPoliciesResponse
    -- ** Response lenses
    , lkprNextMarker
    , lkprPolicyNames
    , lkprTruncated
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.KMS.Types
import qualified GHC.Exts

data ListKeyPolicies = ListKeyPolicies
    { _lkpKeyId  :: Text
    , _lkpLimit  :: Maybe Natural
    , _lkpMarker :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListKeyPolicies' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lkpKeyId' @::@ 'Text'
--
-- * 'lkpLimit' @::@ 'Maybe' 'Natural'
--
-- * 'lkpMarker' @::@ 'Maybe' 'Text'
--
listKeyPolicies :: Text -- ^ 'lkpKeyId'
                -> ListKeyPolicies
listKeyPolicies p1 = ListKeyPolicies
    { _lkpKeyId  = p1
    , _lkpLimit  = Nothing
    , _lkpMarker = Nothing
    }

-- | Unique identifier of the key. This can be an ARN, an alias, or a globally
-- unique identifier.
lkpKeyId :: Lens' ListKeyPolicies Text
lkpKeyId = lens _lkpKeyId (\s a -> s { _lkpKeyId = a })

-- | Specify this parameter only when paginating results to indicate the
-- maximum number of policies you want listed in the response. If there are
-- additional policies beyond the maximum you specify, the Truncated
-- response element will be set to true.
lkpLimit :: Lens' ListKeyPolicies (Maybe Natural)
lkpLimit = lens _lkpLimit (\s a -> s { _lkpLimit = a })

-- | Use this parameter only when paginating results, and only in a subsequent
-- request after you've received a response where the results are truncated.
-- Set it to the value of the NextMarker in the response you just received.
lkpMarker :: Lens' ListKeyPolicies (Maybe Text)
lkpMarker = lens _lkpMarker (\s a -> s { _lkpMarker = a })

instance ToPath ListKeyPolicies where
    toPath = const "/"

instance ToQuery ListKeyPolicies where
    toQuery = const mempty

instance ToHeaders ListKeyPolicies

instance ToBody ListKeyPolicies where
    toBody = toBody . encode . _lkpKeyId

data ListKeyPoliciesResponse = ListKeyPoliciesResponse
    { _lkprNextMarker  :: Maybe Text
    , _lkprPolicyNames :: [Text]
    , _lkprTruncated   :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListKeyPoliciesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lkprNextMarker' @::@ 'Maybe' 'Text'
--
-- * 'lkprPolicyNames' @::@ ['Text']
--
-- * 'lkprTruncated' @::@ 'Maybe' 'Bool'
--
listKeyPoliciesResponse :: ListKeyPoliciesResponse
listKeyPoliciesResponse = ListKeyPoliciesResponse
    { _lkprPolicyNames = mempty
    , _lkprNextMarker  = Nothing
    , _lkprTruncated   = Nothing
    }

-- | If Truncated is true, this value is present and contains the value to use
-- for the Marker request parameter in a subsequent pagination request.
lkprNextMarker :: Lens' ListKeyPoliciesResponse (Maybe Text)
lkprNextMarker = lens _lkprNextMarker (\s a -> s { _lkprNextMarker = a })

-- | A list of policy names. Currently, there is only one policy and it is
-- named "Default".
lkprPolicyNames :: Lens' ListKeyPoliciesResponse [Text]
lkprPolicyNames = lens _lkprPolicyNames (\s a -> s { _lkprPolicyNames = a })

-- | A flag that indicates whether there are more items in the list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the Marker request parameter to retrieve more policies in the list.
lkprTruncated :: Lens' ListKeyPoliciesResponse (Maybe Bool)
lkprTruncated = lens _lkprTruncated (\s a -> s { _lkprTruncated = a })

instance AWSRequest ListKeyPolicies where
    type Sv ListKeyPolicies = KMS
    type Rs ListKeyPolicies = ListKeyPoliciesResponse

    request  = post
    response = jsonResponse $ \h o -> ListKeyPoliciesResponse
        <$> o .: "NextMarker"
        <*> o .: "PolicyNames"
        <*> o .: "Truncated"
