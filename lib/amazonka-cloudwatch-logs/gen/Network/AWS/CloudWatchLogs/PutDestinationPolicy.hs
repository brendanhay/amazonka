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
-- Module      : Network.AWS.CloudWatchLogs.PutDestinationPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates an access policy associated with an existing destination. An access policy is an <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies_overview.html IAM policy document> that is used to authorize claims to register a subscription filter against a given destination.
--
--
module Network.AWS.CloudWatchLogs.PutDestinationPolicy
    (
    -- * Creating a Request
      putDestinationPolicy
    , PutDestinationPolicy
    -- * Request Lenses
    , pdpDestinationName
    , pdpAccessPolicy

    -- * Destructuring the Response
    , putDestinationPolicyResponse
    , PutDestinationPolicyResponse
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putDestinationPolicy' smart constructor.
data PutDestinationPolicy = PutDestinationPolicy'
  { _pdpDestinationName :: !Text
  , _pdpAccessPolicy    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutDestinationPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdpDestinationName' - A name for an existing destination.
--
-- * 'pdpAccessPolicy' - An IAM policy document that authorizes cross-account users to deliver their log events to the associated destination.
putDestinationPolicy
    :: Text -- ^ 'pdpDestinationName'
    -> Text -- ^ 'pdpAccessPolicy'
    -> PutDestinationPolicy
putDestinationPolicy pDestinationName_ pAccessPolicy_ =
  PutDestinationPolicy'
    {_pdpDestinationName = pDestinationName_, _pdpAccessPolicy = pAccessPolicy_}


-- | A name for an existing destination.
pdpDestinationName :: Lens' PutDestinationPolicy Text
pdpDestinationName = lens _pdpDestinationName (\ s a -> s{_pdpDestinationName = a})

-- | An IAM policy document that authorizes cross-account users to deliver their log events to the associated destination.
pdpAccessPolicy :: Lens' PutDestinationPolicy Text
pdpAccessPolicy = lens _pdpAccessPolicy (\ s a -> s{_pdpAccessPolicy = a})

instance AWSRequest PutDestinationPolicy where
        type Rs PutDestinationPolicy =
             PutDestinationPolicyResponse
        request = postJSON cloudWatchLogs
        response = receiveNull PutDestinationPolicyResponse'

instance Hashable PutDestinationPolicy where

instance NFData PutDestinationPolicy where

instance ToHeaders PutDestinationPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.PutDestinationPolicy" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutDestinationPolicy where
        toJSON PutDestinationPolicy'{..}
          = object
              (catMaybes
                 [Just ("destinationName" .= _pdpDestinationName),
                  Just ("accessPolicy" .= _pdpAccessPolicy)])

instance ToPath PutDestinationPolicy where
        toPath = const "/"

instance ToQuery PutDestinationPolicy where
        toQuery = const mempty

-- | /See:/ 'putDestinationPolicyResponse' smart constructor.
data PutDestinationPolicyResponse =
  PutDestinationPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutDestinationPolicyResponse' with the minimum fields required to make a request.
--
putDestinationPolicyResponse
    :: PutDestinationPolicyResponse
putDestinationPolicyResponse = PutDestinationPolicyResponse'


instance NFData PutDestinationPolicyResponse where
