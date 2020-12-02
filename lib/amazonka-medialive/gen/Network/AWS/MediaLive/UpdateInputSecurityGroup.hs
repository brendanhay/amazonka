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
-- Module      : Network.AWS.MediaLive.UpdateInputSecurityGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an Input Security Group's Whilelists.
module Network.AWS.MediaLive.UpdateInputSecurityGroup
    (
    -- * Creating a Request
      updateInputSecurityGroup
    , UpdateInputSecurityGroup
    -- * Request Lenses
    , uisgWhitelistRules
    , uisgInputSecurityGroupId

    -- * Destructuring the Response
    , updateInputSecurityGroupResponse
    , UpdateInputSecurityGroupResponse
    -- * Response Lenses
    , uisgrsSecurityGroup
    , uisgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.MediaLive.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to update some combination of the Input Security Group name and the IPv4 CIDRs the Input Security Group should allow.
--
-- /See:/ 'updateInputSecurityGroup' smart constructor.
data UpdateInputSecurityGroup = UpdateInputSecurityGroup'
  { _uisgWhitelistRules       :: !(Maybe [InputWhitelistRuleCidr])
  , _uisgInputSecurityGroupId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateInputSecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uisgWhitelistRules' - List of IPv4 CIDR addresses to whitelist
--
-- * 'uisgInputSecurityGroupId' - The id of the Input Security Group to update.
updateInputSecurityGroup
    :: Text -- ^ 'uisgInputSecurityGroupId'
    -> UpdateInputSecurityGroup
updateInputSecurityGroup pInputSecurityGroupId_ =
  UpdateInputSecurityGroup'
    { _uisgWhitelistRules = Nothing
    , _uisgInputSecurityGroupId = pInputSecurityGroupId_
    }


-- | List of IPv4 CIDR addresses to whitelist
uisgWhitelistRules :: Lens' UpdateInputSecurityGroup [InputWhitelistRuleCidr]
uisgWhitelistRules = lens _uisgWhitelistRules (\ s a -> s{_uisgWhitelistRules = a}) . _Default . _Coerce

-- | The id of the Input Security Group to update.
uisgInputSecurityGroupId :: Lens' UpdateInputSecurityGroup Text
uisgInputSecurityGroupId = lens _uisgInputSecurityGroupId (\ s a -> s{_uisgInputSecurityGroupId = a})

instance AWSRequest UpdateInputSecurityGroup where
        type Rs UpdateInputSecurityGroup =
             UpdateInputSecurityGroupResponse
        request = putJSON mediaLive
        response
          = receiveJSON
              (\ s h x ->
                 UpdateInputSecurityGroupResponse' <$>
                   (x .?> "securityGroup") <*> (pure (fromEnum s)))

instance Hashable UpdateInputSecurityGroup where

instance NFData UpdateInputSecurityGroup where

instance ToHeaders UpdateInputSecurityGroup where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateInputSecurityGroup where
        toJSON UpdateInputSecurityGroup'{..}
          = object
              (catMaybes
                 [("whitelistRules" .=) <$> _uisgWhitelistRules])

instance ToPath UpdateInputSecurityGroup where
        toPath UpdateInputSecurityGroup'{..}
          = mconcat
              ["/prod/inputSecurityGroups/",
               toBS _uisgInputSecurityGroupId]

instance ToQuery UpdateInputSecurityGroup where
        toQuery = const mempty

-- | Placeholder documentation for UpdateInputSecurityGroupResponse
--
-- /See:/ 'updateInputSecurityGroupResponse' smart constructor.
data UpdateInputSecurityGroupResponse = UpdateInputSecurityGroupResponse'
  { _uisgrsSecurityGroup  :: !(Maybe InputSecurityGroup)
  , _uisgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateInputSecurityGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uisgrsSecurityGroup' - Undocumented member.
--
-- * 'uisgrsResponseStatus' - -- | The response status code.
updateInputSecurityGroupResponse
    :: Int -- ^ 'uisgrsResponseStatus'
    -> UpdateInputSecurityGroupResponse
updateInputSecurityGroupResponse pResponseStatus_ =
  UpdateInputSecurityGroupResponse'
    {_uisgrsSecurityGroup = Nothing, _uisgrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
uisgrsSecurityGroup :: Lens' UpdateInputSecurityGroupResponse (Maybe InputSecurityGroup)
uisgrsSecurityGroup = lens _uisgrsSecurityGroup (\ s a -> s{_uisgrsSecurityGroup = a})

-- | -- | The response status code.
uisgrsResponseStatus :: Lens' UpdateInputSecurityGroupResponse Int
uisgrsResponseStatus = lens _uisgrsResponseStatus (\ s a -> s{_uisgrsResponseStatus = a})

instance NFData UpdateInputSecurityGroupResponse
         where
