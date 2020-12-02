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
-- Module      : Network.AWS.MediaLive.CreateInputSecurityGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Input Security Group
module Network.AWS.MediaLive.CreateInputSecurityGroup
    (
    -- * Creating a Request
      createInputSecurityGroup
    , CreateInputSecurityGroup
    -- * Request Lenses
    , cisgWhitelistRules

    -- * Destructuring the Response
    , createInputSecurityGroupResponse
    , CreateInputSecurityGroupResponse
    -- * Response Lenses
    , cisgrsSecurityGroup
    , cisgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.MediaLive.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The IPv4 CIDRs to whitelist for this Input Security Group
--
-- /See:/ 'createInputSecurityGroup' smart constructor.
newtype CreateInputSecurityGroup = CreateInputSecurityGroup'
  { _cisgWhitelistRules :: Maybe [InputWhitelistRuleCidr]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateInputSecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cisgWhitelistRules' - List of IPv4 CIDR addresses to whitelist
createInputSecurityGroup
    :: CreateInputSecurityGroup
createInputSecurityGroup =
  CreateInputSecurityGroup' {_cisgWhitelistRules = Nothing}


-- | List of IPv4 CIDR addresses to whitelist
cisgWhitelistRules :: Lens' CreateInputSecurityGroup [InputWhitelistRuleCidr]
cisgWhitelistRules = lens _cisgWhitelistRules (\ s a -> s{_cisgWhitelistRules = a}) . _Default . _Coerce

instance AWSRequest CreateInputSecurityGroup where
        type Rs CreateInputSecurityGroup =
             CreateInputSecurityGroupResponse
        request = postJSON mediaLive
        response
          = receiveJSON
              (\ s h x ->
                 CreateInputSecurityGroupResponse' <$>
                   (x .?> "securityGroup") <*> (pure (fromEnum s)))

instance Hashable CreateInputSecurityGroup where

instance NFData CreateInputSecurityGroup where

instance ToHeaders CreateInputSecurityGroup where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateInputSecurityGroup where
        toJSON CreateInputSecurityGroup'{..}
          = object
              (catMaybes
                 [("whitelistRules" .=) <$> _cisgWhitelistRules])

instance ToPath CreateInputSecurityGroup where
        toPath = const "/prod/inputSecurityGroups"

instance ToQuery CreateInputSecurityGroup where
        toQuery = const mempty

-- | Placeholder documentation for CreateInputSecurityGroupResponse
--
-- /See:/ 'createInputSecurityGroupResponse' smart constructor.
data CreateInputSecurityGroupResponse = CreateInputSecurityGroupResponse'
  { _cisgrsSecurityGroup  :: !(Maybe InputSecurityGroup)
  , _cisgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateInputSecurityGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cisgrsSecurityGroup' - Undocumented member.
--
-- * 'cisgrsResponseStatus' - -- | The response status code.
createInputSecurityGroupResponse
    :: Int -- ^ 'cisgrsResponseStatus'
    -> CreateInputSecurityGroupResponse
createInputSecurityGroupResponse pResponseStatus_ =
  CreateInputSecurityGroupResponse'
    {_cisgrsSecurityGroup = Nothing, _cisgrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
cisgrsSecurityGroup :: Lens' CreateInputSecurityGroupResponse (Maybe InputSecurityGroup)
cisgrsSecurityGroup = lens _cisgrsSecurityGroup (\ s a -> s{_cisgrsSecurityGroup = a})

-- | -- | The response status code.
cisgrsResponseStatus :: Lens' CreateInputSecurityGroupResponse Int
cisgrsResponseStatus = lens _cisgrsResponseStatus (\ s a -> s{_cisgrsResponseStatus = a})

instance NFData CreateInputSecurityGroupResponse
         where
