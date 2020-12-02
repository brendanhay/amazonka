{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.GetAccessControlEffect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the effects of an organization's access control rules as they apply to a specified IPv4 address, access protocol action, or user ID.
module Network.AWS.WorkMail.GetAccessControlEffect
  ( -- * Creating a Request
    getAccessControlEffect,
    GetAccessControlEffect,

    -- * Request Lenses
    gaceOrganizationId,
    gaceIPAddress,
    gaceAction,
    gaceUserId,

    -- * Destructuring the Response
    getAccessControlEffectResponse,
    GetAccessControlEffectResponse,

    -- * Response Lenses
    gacersEffect,
    gacersMatchedRules,
    gacersResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'getAccessControlEffect' smart constructor.
data GetAccessControlEffect = GetAccessControlEffect'
  { _gaceOrganizationId ::
      !Text,
    _gaceIPAddress :: !Text,
    _gaceAction :: !Text,
    _gaceUserId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetAccessControlEffect' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaceOrganizationId' - The identifier for the organization.
--
-- * 'gaceIPAddress' - The IPv4 address.
--
-- * 'gaceAction' - The access protocol action. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
--
-- * 'gaceUserId' - The user ID.
getAccessControlEffect ::
  -- | 'gaceOrganizationId'
  Text ->
  -- | 'gaceIPAddress'
  Text ->
  -- | 'gaceAction'
  Text ->
  -- | 'gaceUserId'
  Text ->
  GetAccessControlEffect
getAccessControlEffect
  pOrganizationId_
  pIPAddress_
  pAction_
  pUserId_ =
    GetAccessControlEffect'
      { _gaceOrganizationId = pOrganizationId_,
        _gaceIPAddress = pIPAddress_,
        _gaceAction = pAction_,
        _gaceUserId = pUserId_
      }

-- | The identifier for the organization.
gaceOrganizationId :: Lens' GetAccessControlEffect Text
gaceOrganizationId = lens _gaceOrganizationId (\s a -> s {_gaceOrganizationId = a})

-- | The IPv4 address.
gaceIPAddress :: Lens' GetAccessControlEffect Text
gaceIPAddress = lens _gaceIPAddress (\s a -> s {_gaceIPAddress = a})

-- | The access protocol action. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
gaceAction :: Lens' GetAccessControlEffect Text
gaceAction = lens _gaceAction (\s a -> s {_gaceAction = a})

-- | The user ID.
gaceUserId :: Lens' GetAccessControlEffect Text
gaceUserId = lens _gaceUserId (\s a -> s {_gaceUserId = a})

instance AWSRequest GetAccessControlEffect where
  type Rs GetAccessControlEffect = GetAccessControlEffectResponse
  request = postJSON workMail
  response =
    receiveJSON
      ( \s h x ->
          GetAccessControlEffectResponse'
            <$> (x .?> "Effect")
            <*> (x .?> "MatchedRules" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetAccessControlEffect

instance NFData GetAccessControlEffect

instance ToHeaders GetAccessControlEffect where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkMailService.GetAccessControlEffect" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetAccessControlEffect where
  toJSON GetAccessControlEffect' {..} =
    object
      ( catMaybes
          [ Just ("OrganizationId" .= _gaceOrganizationId),
            Just ("IpAddress" .= _gaceIPAddress),
            Just ("Action" .= _gaceAction),
            Just ("UserId" .= _gaceUserId)
          ]
      )

instance ToPath GetAccessControlEffect where
  toPath = const "/"

instance ToQuery GetAccessControlEffect where
  toQuery = const mempty

-- | /See:/ 'getAccessControlEffectResponse' smart constructor.
data GetAccessControlEffectResponse = GetAccessControlEffectResponse'
  { _gacersEffect ::
      !( Maybe
           AccessControlRuleEffect
       ),
    _gacersMatchedRules ::
      !(Maybe [Text]),
    _gacersResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetAccessControlEffectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gacersEffect' - The rule effect.
--
-- * 'gacersMatchedRules' - The rules that match the given parameters, resulting in an effect.
--
-- * 'gacersResponseStatus' - -- | The response status code.
getAccessControlEffectResponse ::
  -- | 'gacersResponseStatus'
  Int ->
  GetAccessControlEffectResponse
getAccessControlEffectResponse pResponseStatus_ =
  GetAccessControlEffectResponse'
    { _gacersEffect = Nothing,
      _gacersMatchedRules = Nothing,
      _gacersResponseStatus = pResponseStatus_
    }

-- | The rule effect.
gacersEffect :: Lens' GetAccessControlEffectResponse (Maybe AccessControlRuleEffect)
gacersEffect = lens _gacersEffect (\s a -> s {_gacersEffect = a})

-- | The rules that match the given parameters, resulting in an effect.
gacersMatchedRules :: Lens' GetAccessControlEffectResponse [Text]
gacersMatchedRules = lens _gacersMatchedRules (\s a -> s {_gacersMatchedRules = a}) . _Default . _Coerce

-- | -- | The response status code.
gacersResponseStatus :: Lens' GetAccessControlEffectResponse Int
gacersResponseStatus = lens _gacersResponseStatus (\s a -> s {_gacersResponseStatus = a})

instance NFData GetAccessControlEffectResponse
