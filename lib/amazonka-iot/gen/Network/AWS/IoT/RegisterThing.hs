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
-- Module      : Network.AWS.IoT.RegisterThing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions a thing in the device registry. RegisterThing calls other AWS IoT control plane APIs. These calls might exceed your account level <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_iot AWS IoT Throttling Limits> and cause throttle errors. Please contact <https://console.aws.amazon.com/support/home AWS Customer Support> to raise your throttling limits if necessary.
module Network.AWS.IoT.RegisterThing
  ( -- * Creating a Request
    registerThing,
    RegisterThing,

    -- * Request Lenses
    rtParameters,
    rtTemplateBody,

    -- * Destructuring the Response
    registerThingResponse,
    RegisterThingResponse,

    -- * Response Lenses
    rtrsCertificatePem,
    rtrsResourceARNs,
    rtrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'registerThing' smart constructor.
data RegisterThing = RegisterThing'
  { _rtParameters ::
      !(Maybe (Map Text (Text))),
    _rtTemplateBody :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterThing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtParameters' - The parameters for provisioning a thing. See <https://docs.aws.amazon.com/iot/latest/developerguide/provision-template.html Provisioning Templates> for more information.
--
-- * 'rtTemplateBody' - The provisioning template. See <https://docs.aws.amazon.com/iot/latest/developerguide/provision-w-cert.html Provisioning Devices That Have Device Certificates> for more information.
registerThing ::
  -- | 'rtTemplateBody'
  Text ->
  RegisterThing
registerThing pTemplateBody_ =
  RegisterThing'
    { _rtParameters = Nothing,
      _rtTemplateBody = pTemplateBody_
    }

-- | The parameters for provisioning a thing. See <https://docs.aws.amazon.com/iot/latest/developerguide/provision-template.html Provisioning Templates> for more information.
rtParameters :: Lens' RegisterThing (HashMap Text (Text))
rtParameters = lens _rtParameters (\s a -> s {_rtParameters = a}) . _Default . _Map

-- | The provisioning template. See <https://docs.aws.amazon.com/iot/latest/developerguide/provision-w-cert.html Provisioning Devices That Have Device Certificates> for more information.
rtTemplateBody :: Lens' RegisterThing Text
rtTemplateBody = lens _rtTemplateBody (\s a -> s {_rtTemplateBody = a})

instance AWSRequest RegisterThing where
  type Rs RegisterThing = RegisterThingResponse
  request = postJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          RegisterThingResponse'
            <$> (x .?> "certificatePem")
            <*> (x .?> "resourceArns" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable RegisterThing

instance NFData RegisterThing

instance ToHeaders RegisterThing where
  toHeaders = const mempty

instance ToJSON RegisterThing where
  toJSON RegisterThing' {..} =
    object
      ( catMaybes
          [ ("parameters" .=) <$> _rtParameters,
            Just ("templateBody" .= _rtTemplateBody)
          ]
      )

instance ToPath RegisterThing where
  toPath = const "/things"

instance ToQuery RegisterThing where
  toQuery = const mempty

-- | /See:/ 'registerThingResponse' smart constructor.
data RegisterThingResponse = RegisterThingResponse'
  { _rtrsCertificatePem ::
      !(Maybe Text),
    _rtrsResourceARNs :: !(Maybe (Map Text (Text))),
    _rtrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterThingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtrsCertificatePem' - The certificate data, in PEM format.
--
-- * 'rtrsResourceARNs' - ARNs for the generated resources.
--
-- * 'rtrsResponseStatus' - -- | The response status code.
registerThingResponse ::
  -- | 'rtrsResponseStatus'
  Int ->
  RegisterThingResponse
registerThingResponse pResponseStatus_ =
  RegisterThingResponse'
    { _rtrsCertificatePem = Nothing,
      _rtrsResourceARNs = Nothing,
      _rtrsResponseStatus = pResponseStatus_
    }

-- | The certificate data, in PEM format.
rtrsCertificatePem :: Lens' RegisterThingResponse (Maybe Text)
rtrsCertificatePem = lens _rtrsCertificatePem (\s a -> s {_rtrsCertificatePem = a})

-- | ARNs for the generated resources.
rtrsResourceARNs :: Lens' RegisterThingResponse (HashMap Text (Text))
rtrsResourceARNs = lens _rtrsResourceARNs (\s a -> s {_rtrsResourceARNs = a}) . _Default . _Map

-- | -- | The response status code.
rtrsResponseStatus :: Lens' RegisterThingResponse Int
rtrsResponseStatus = lens _rtrsResponseStatus (\s a -> s {_rtrsResponseStatus = a})

instance NFData RegisterThingResponse
