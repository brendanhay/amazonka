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
-- Module      : Network.AWS.Glacier.SetDataRetrievalPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation sets and then enacts a data retrieval policy in the region specified in the PUT request. You can set one policy per region for an AWS account. The policy is enacted within a few minutes of a successful PUT operation.
--
--
-- The set policy operation does not affect retrieval jobs that were in progress before the policy was enacted. For more information about data retrieval policies, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/data-retrieval-policy.html Amazon Glacier Data Retrieval Policies> .
--
module Network.AWS.Glacier.SetDataRetrievalPolicy
    (
    -- * Creating a Request
      setDataRetrievalPolicy
    , SetDataRetrievalPolicy
    -- * Request Lenses
    , sdrpPolicy
    , sdrpAccountId

    -- * Destructuring the Response
    , setDataRetrievalPolicyResponse
    , SetDataRetrievalPolicyResponse
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Glacier.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | SetDataRetrievalPolicy input.
--
--
--
-- /See:/ 'setDataRetrievalPolicy' smart constructor.
data SetDataRetrievalPolicy = SetDataRetrievalPolicy'
  { _sdrpPolicy    :: !(Maybe DataRetrievalPolicy)
  , _sdrpAccountId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetDataRetrievalPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdrpPolicy' - The data retrieval policy in JSON format.
--
-- * 'sdrpAccountId' - The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
setDataRetrievalPolicy
    :: Text -- ^ 'sdrpAccountId'
    -> SetDataRetrievalPolicy
setDataRetrievalPolicy pAccountId_ =
  SetDataRetrievalPolicy' {_sdrpPolicy = Nothing, _sdrpAccountId = pAccountId_}


-- | The data retrieval policy in JSON format.
sdrpPolicy :: Lens' SetDataRetrievalPolicy (Maybe DataRetrievalPolicy)
sdrpPolicy = lens _sdrpPolicy (\ s a -> s{_sdrpPolicy = a})

-- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
sdrpAccountId :: Lens' SetDataRetrievalPolicy Text
sdrpAccountId = lens _sdrpAccountId (\ s a -> s{_sdrpAccountId = a})

instance AWSRequest SetDataRetrievalPolicy where
        type Rs SetDataRetrievalPolicy =
             SetDataRetrievalPolicyResponse
        request = putJSON glacier
        response
          = receiveNull SetDataRetrievalPolicyResponse'

instance Hashable SetDataRetrievalPolicy where

instance NFData SetDataRetrievalPolicy where

instance ToHeaders SetDataRetrievalPolicy where
        toHeaders = const mempty

instance ToJSON SetDataRetrievalPolicy where
        toJSON SetDataRetrievalPolicy'{..}
          = object (catMaybes [("Policy" .=) <$> _sdrpPolicy])

instance ToPath SetDataRetrievalPolicy where
        toPath SetDataRetrievalPolicy'{..}
          = mconcat
              ["/", toBS _sdrpAccountId,
               "/policies/data-retrieval"]

instance ToQuery SetDataRetrievalPolicy where
        toQuery = const mempty

-- | /See:/ 'setDataRetrievalPolicyResponse' smart constructor.
data SetDataRetrievalPolicyResponse =
  SetDataRetrievalPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetDataRetrievalPolicyResponse' with the minimum fields required to make a request.
--
setDataRetrievalPolicyResponse
    :: SetDataRetrievalPolicyResponse
setDataRetrievalPolicyResponse = SetDataRetrievalPolicyResponse'


instance NFData SetDataRetrievalPolicyResponse where
