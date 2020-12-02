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
-- Module      : Network.AWS.MediaLive.CreateInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an input
module Network.AWS.MediaLive.CreateInput
  ( -- * Creating a Request
    createInput,
    CreateInput,

    -- * Request Lenses
    ciRequestId,
    ciInputDevices,
    ciSources,
    ciInputSecurityGroups,
    ciDestinations,
    ciName,
    ciVPC,
    ciType,
    ciMediaConnectFlows,
    ciTags,
    ciRoleARN,

    -- * Destructuring the Response
    createInputResponse,
    CreateInputResponse,

    -- * Response Lenses
    cirsInput,
    cirsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The name of the input
--
-- /See:/ 'createInput' smart constructor.
data CreateInput = CreateInput'
  { _ciRequestId :: !(Maybe Text),
    _ciInputDevices :: !(Maybe [InputDeviceSettings]),
    _ciSources :: !(Maybe [InputSourceRequest]),
    _ciInputSecurityGroups :: !(Maybe [Text]),
    _ciDestinations :: !(Maybe [InputDestinationRequest]),
    _ciName :: !(Maybe Text),
    _ciVPC :: !(Maybe InputVPCRequest),
    _ciType :: !(Maybe InputType),
    _ciMediaConnectFlows :: !(Maybe [MediaConnectFlowRequest]),
    _ciTags :: !(Maybe (Map Text (Text))),
    _ciRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciRequestId' - Unique identifier of the request to ensure the request is handled exactly once in case of retries.
--
-- * 'ciInputDevices' - Settings for the devices.
--
-- * 'ciSources' - The source URLs for a PULL-type input. Every PULL type input needs exactly two source URLs for redundancy. Only specify sources for PULL type Inputs. Leave Destinations empty.
--
-- * 'ciInputSecurityGroups' - A list of security groups referenced by IDs to attach to the input.
--
-- * 'ciDestinations' - Destination settings for PUSH type inputs.
--
-- * 'ciName' - Name of the input.
--
-- * 'ciVPC' - Undocumented member.
--
-- * 'ciType' - Undocumented member.
--
-- * 'ciMediaConnectFlows' - A list of the MediaConnect Flows that you want to use in this input. You can specify as few as one Flow and presently, as many as two. The only requirement is when you have more than one is that each Flow is in a separate Availability Zone as this ensures your EML input is redundant to AZ issues.
--
-- * 'ciTags' - A collection of key-value pairs.
--
-- * 'ciRoleARN' - The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
createInput ::
  CreateInput
createInput =
  CreateInput'
    { _ciRequestId = Nothing,
      _ciInputDevices = Nothing,
      _ciSources = Nothing,
      _ciInputSecurityGroups = Nothing,
      _ciDestinations = Nothing,
      _ciName = Nothing,
      _ciVPC = Nothing,
      _ciType = Nothing,
      _ciMediaConnectFlows = Nothing,
      _ciTags = Nothing,
      _ciRoleARN = Nothing
    }

-- | Unique identifier of the request to ensure the request is handled exactly once in case of retries.
ciRequestId :: Lens' CreateInput (Maybe Text)
ciRequestId = lens _ciRequestId (\s a -> s {_ciRequestId = a})

-- | Settings for the devices.
ciInputDevices :: Lens' CreateInput [InputDeviceSettings]
ciInputDevices = lens _ciInputDevices (\s a -> s {_ciInputDevices = a}) . _Default . _Coerce

-- | The source URLs for a PULL-type input. Every PULL type input needs exactly two source URLs for redundancy. Only specify sources for PULL type Inputs. Leave Destinations empty.
ciSources :: Lens' CreateInput [InputSourceRequest]
ciSources = lens _ciSources (\s a -> s {_ciSources = a}) . _Default . _Coerce

-- | A list of security groups referenced by IDs to attach to the input.
ciInputSecurityGroups :: Lens' CreateInput [Text]
ciInputSecurityGroups = lens _ciInputSecurityGroups (\s a -> s {_ciInputSecurityGroups = a}) . _Default . _Coerce

-- | Destination settings for PUSH type inputs.
ciDestinations :: Lens' CreateInput [InputDestinationRequest]
ciDestinations = lens _ciDestinations (\s a -> s {_ciDestinations = a}) . _Default . _Coerce

-- | Name of the input.
ciName :: Lens' CreateInput (Maybe Text)
ciName = lens _ciName (\s a -> s {_ciName = a})

-- | Undocumented member.
ciVPC :: Lens' CreateInput (Maybe InputVPCRequest)
ciVPC = lens _ciVPC (\s a -> s {_ciVPC = a})

-- | Undocumented member.
ciType :: Lens' CreateInput (Maybe InputType)
ciType = lens _ciType (\s a -> s {_ciType = a})

-- | A list of the MediaConnect Flows that you want to use in this input. You can specify as few as one Flow and presently, as many as two. The only requirement is when you have more than one is that each Flow is in a separate Availability Zone as this ensures your EML input is redundant to AZ issues.
ciMediaConnectFlows :: Lens' CreateInput [MediaConnectFlowRequest]
ciMediaConnectFlows = lens _ciMediaConnectFlows (\s a -> s {_ciMediaConnectFlows = a}) . _Default . _Coerce

-- | A collection of key-value pairs.
ciTags :: Lens' CreateInput (HashMap Text (Text))
ciTags = lens _ciTags (\s a -> s {_ciTags = a}) . _Default . _Map

-- | The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
ciRoleARN :: Lens' CreateInput (Maybe Text)
ciRoleARN = lens _ciRoleARN (\s a -> s {_ciRoleARN = a})

instance AWSRequest CreateInput where
  type Rs CreateInput = CreateInputResponse
  request = postJSON mediaLive
  response =
    receiveJSON
      ( \s h x ->
          CreateInputResponse' <$> (x .?> "input") <*> (pure (fromEnum s))
      )

instance Hashable CreateInput

instance NFData CreateInput

instance ToHeaders CreateInput where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON CreateInput where
  toJSON CreateInput' {..} =
    object
      ( catMaybes
          [ ("requestId" .=) <$> _ciRequestId,
            ("inputDevices" .=) <$> _ciInputDevices,
            ("sources" .=) <$> _ciSources,
            ("inputSecurityGroups" .=) <$> _ciInputSecurityGroups,
            ("destinations" .=) <$> _ciDestinations,
            ("name" .=) <$> _ciName,
            ("vpc" .=) <$> _ciVPC,
            ("type" .=) <$> _ciType,
            ("mediaConnectFlows" .=) <$> _ciMediaConnectFlows,
            ("tags" .=) <$> _ciTags,
            ("roleArn" .=) <$> _ciRoleARN
          ]
      )

instance ToPath CreateInput where
  toPath = const "/prod/inputs"

instance ToQuery CreateInput where
  toQuery = const mempty

-- | Placeholder documentation for CreateInputResponse
--
-- /See:/ 'createInputResponse' smart constructor.
data CreateInputResponse = CreateInputResponse'
  { _cirsInput ::
      !(Maybe Input),
    _cirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateInputResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cirsInput' - Undocumented member.
--
-- * 'cirsResponseStatus' - -- | The response status code.
createInputResponse ::
  -- | 'cirsResponseStatus'
  Int ->
  CreateInputResponse
createInputResponse pResponseStatus_ =
  CreateInputResponse'
    { _cirsInput = Nothing,
      _cirsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
cirsInput :: Lens' CreateInputResponse (Maybe Input)
cirsInput = lens _cirsInput (\s a -> s {_cirsInput = a})

-- | -- | The response status code.
cirsResponseStatus :: Lens' CreateInputResponse Int
cirsResponseStatus = lens _cirsResponseStatus (\s a -> s {_cirsResponseStatus = a})

instance NFData CreateInputResponse
