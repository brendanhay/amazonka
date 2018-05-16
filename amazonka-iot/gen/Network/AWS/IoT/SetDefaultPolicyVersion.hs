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
-- Module      : Network.AWS.IoT.SetDefaultPolicyVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the specified version of the specified policy as the policy's default (operative) version. This action affects all certificates to which the policy is attached. To list the principals the policy is attached to, use the ListPrincipalPolicy API.
--
--
module Network.AWS.IoT.SetDefaultPolicyVersion
    (
    -- * Creating a Request
      setDefaultPolicyVersion
    , SetDefaultPolicyVersion
    -- * Request Lenses
    , sdpvPolicyName
    , sdpvPolicyVersionId

    -- * Destructuring the Response
    , setDefaultPolicyVersionResponse
    , SetDefaultPolicyVersionResponse
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the SetDefaultPolicyVersion operation.
--
--
--
-- /See:/ 'setDefaultPolicyVersion' smart constructor.
data SetDefaultPolicyVersion = SetDefaultPolicyVersion'
  { _sdpvPolicyName      :: !Text
  , _sdpvPolicyVersionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetDefaultPolicyVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdpvPolicyName' - The policy name.
--
-- * 'sdpvPolicyVersionId' - The policy version ID.
setDefaultPolicyVersion
    :: Text -- ^ 'sdpvPolicyName'
    -> Text -- ^ 'sdpvPolicyVersionId'
    -> SetDefaultPolicyVersion
setDefaultPolicyVersion pPolicyName_ pPolicyVersionId_ =
  SetDefaultPolicyVersion'
    {_sdpvPolicyName = pPolicyName_, _sdpvPolicyVersionId = pPolicyVersionId_}


-- | The policy name.
sdpvPolicyName :: Lens' SetDefaultPolicyVersion Text
sdpvPolicyName = lens _sdpvPolicyName (\ s a -> s{_sdpvPolicyName = a})

-- | The policy version ID.
sdpvPolicyVersionId :: Lens' SetDefaultPolicyVersion Text
sdpvPolicyVersionId = lens _sdpvPolicyVersionId (\ s a -> s{_sdpvPolicyVersionId = a})

instance AWSRequest SetDefaultPolicyVersion where
        type Rs SetDefaultPolicyVersion =
             SetDefaultPolicyVersionResponse
        request = patchJSON ioT
        response
          = receiveNull SetDefaultPolicyVersionResponse'

instance Hashable SetDefaultPolicyVersion where

instance NFData SetDefaultPolicyVersion where

instance ToHeaders SetDefaultPolicyVersion where
        toHeaders = const mempty

instance ToJSON SetDefaultPolicyVersion where
        toJSON = const (Object mempty)

instance ToPath SetDefaultPolicyVersion where
        toPath SetDefaultPolicyVersion'{..}
          = mconcat
              ["/policies/", toBS _sdpvPolicyName, "/version/",
               toBS _sdpvPolicyVersionId]

instance ToQuery SetDefaultPolicyVersion where
        toQuery = const mempty

-- | /See:/ 'setDefaultPolicyVersionResponse' smart constructor.
data SetDefaultPolicyVersionResponse =
  SetDefaultPolicyVersionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetDefaultPolicyVersionResponse' with the minimum fields required to make a request.
--
setDefaultPolicyVersionResponse
    :: SetDefaultPolicyVersionResponse
setDefaultPolicyVersionResponse = SetDefaultPolicyVersionResponse'


instance NFData SetDefaultPolicyVersionResponse where
