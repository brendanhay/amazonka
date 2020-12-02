{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Input
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Input where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.InputClass
import Network.AWS.MediaLive.Types.InputDestination
import Network.AWS.MediaLive.Types.InputDeviceSettings
import Network.AWS.MediaLive.Types.InputSource
import Network.AWS.MediaLive.Types.InputSourceType
import Network.AWS.MediaLive.Types.InputState
import Network.AWS.MediaLive.Types.InputType
import Network.AWS.MediaLive.Types.MediaConnectFlow
import Network.AWS.Prelude

-- | Placeholder documentation for Input
--
-- /See:/ 'input' smart constructor.
data Input = Input'
  { _iState :: !(Maybe InputState),
    _iSecurityGroups :: !(Maybe [Text]),
    _iARN :: !(Maybe Text),
    _iInputDevices :: !(Maybe [InputDeviceSettings]),
    _iSources :: !(Maybe [InputSource]),
    _iDestinations :: !(Maybe [InputDestination]),
    _iName :: !(Maybe Text),
    _iAttachedChannels :: !(Maybe [Text]),
    _iId :: !(Maybe Text),
    _iInputClass :: !(Maybe InputClass),
    _iType :: !(Maybe InputType),
    _iMediaConnectFlows :: !(Maybe [MediaConnectFlow]),
    _iInputSourceType :: !(Maybe InputSourceType),
    _iTags :: !(Maybe (Map Text (Text))),
    _iRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Input' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iState' - Undocumented member.
--
-- * 'iSecurityGroups' - A list of IDs for all the Input Security Groups attached to the input.
--
-- * 'iARN' - The Unique ARN of the input (generated, immutable).
--
-- * 'iInputDevices' - Settings for the input devices.
--
-- * 'iSources' - A list of the sources of the input (PULL-type).
--
-- * 'iDestinations' - A list of the destinations of the input (PUSH-type).
--
-- * 'iName' - The user-assigned name (This is a mutable value).
--
-- * 'iAttachedChannels' - A list of channel IDs that that input is attached to (currently an input can only be attached to one channel).
--
-- * 'iId' - The generated ID of the input (unique for user account, immutable).
--
-- * 'iInputClass' - STANDARD - MediaLive expects two sources to be connected to this input. If the channel is also STANDARD, both sources will be ingested. If the channel is SINGLE_PIPELINE, only the first source will be ingested; the second source will always be ignored, even if the first source fails. SINGLE_PIPELINE - You can connect only one source to this input. If the ChannelClass is also  SINGLE_PIPELINE, this value is valid. If the ChannelClass is STANDARD, this value is not valid because the channel requires two sources in the input.
--
-- * 'iType' - Undocumented member.
--
-- * 'iMediaConnectFlows' - A list of MediaConnect Flows for this input.
--
-- * 'iInputSourceType' - Certain pull input sources can be dynamic, meaning that they can have their URL's dynamically changes during input switch actions. Presently, this functionality only works with MP4_FILE inputs.
--
-- * 'iTags' - A collection of key-value pairs.
--
-- * 'iRoleARN' - The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
input ::
  Input
input =
  Input'
    { _iState = Nothing,
      _iSecurityGroups = Nothing,
      _iARN = Nothing,
      _iInputDevices = Nothing,
      _iSources = Nothing,
      _iDestinations = Nothing,
      _iName = Nothing,
      _iAttachedChannels = Nothing,
      _iId = Nothing,
      _iInputClass = Nothing,
      _iType = Nothing,
      _iMediaConnectFlows = Nothing,
      _iInputSourceType = Nothing,
      _iTags = Nothing,
      _iRoleARN = Nothing
    }

-- | Undocumented member.
iState :: Lens' Input (Maybe InputState)
iState = lens _iState (\s a -> s {_iState = a})

-- | A list of IDs for all the Input Security Groups attached to the input.
iSecurityGroups :: Lens' Input [Text]
iSecurityGroups = lens _iSecurityGroups (\s a -> s {_iSecurityGroups = a}) . _Default . _Coerce

-- | The Unique ARN of the input (generated, immutable).
iARN :: Lens' Input (Maybe Text)
iARN = lens _iARN (\s a -> s {_iARN = a})

-- | Settings for the input devices.
iInputDevices :: Lens' Input [InputDeviceSettings]
iInputDevices = lens _iInputDevices (\s a -> s {_iInputDevices = a}) . _Default . _Coerce

-- | A list of the sources of the input (PULL-type).
iSources :: Lens' Input [InputSource]
iSources = lens _iSources (\s a -> s {_iSources = a}) . _Default . _Coerce

-- | A list of the destinations of the input (PUSH-type).
iDestinations :: Lens' Input [InputDestination]
iDestinations = lens _iDestinations (\s a -> s {_iDestinations = a}) . _Default . _Coerce

-- | The user-assigned name (This is a mutable value).
iName :: Lens' Input (Maybe Text)
iName = lens _iName (\s a -> s {_iName = a})

-- | A list of channel IDs that that input is attached to (currently an input can only be attached to one channel).
iAttachedChannels :: Lens' Input [Text]
iAttachedChannels = lens _iAttachedChannels (\s a -> s {_iAttachedChannels = a}) . _Default . _Coerce

-- | The generated ID of the input (unique for user account, immutable).
iId :: Lens' Input (Maybe Text)
iId = lens _iId (\s a -> s {_iId = a})

-- | STANDARD - MediaLive expects two sources to be connected to this input. If the channel is also STANDARD, both sources will be ingested. If the channel is SINGLE_PIPELINE, only the first source will be ingested; the second source will always be ignored, even if the first source fails. SINGLE_PIPELINE - You can connect only one source to this input. If the ChannelClass is also  SINGLE_PIPELINE, this value is valid. If the ChannelClass is STANDARD, this value is not valid because the channel requires two sources in the input.
iInputClass :: Lens' Input (Maybe InputClass)
iInputClass = lens _iInputClass (\s a -> s {_iInputClass = a})

-- | Undocumented member.
iType :: Lens' Input (Maybe InputType)
iType = lens _iType (\s a -> s {_iType = a})

-- | A list of MediaConnect Flows for this input.
iMediaConnectFlows :: Lens' Input [MediaConnectFlow]
iMediaConnectFlows = lens _iMediaConnectFlows (\s a -> s {_iMediaConnectFlows = a}) . _Default . _Coerce

-- | Certain pull input sources can be dynamic, meaning that they can have their URL's dynamically changes during input switch actions. Presently, this functionality only works with MP4_FILE inputs.
iInputSourceType :: Lens' Input (Maybe InputSourceType)
iInputSourceType = lens _iInputSourceType (\s a -> s {_iInputSourceType = a})

-- | A collection of key-value pairs.
iTags :: Lens' Input (HashMap Text (Text))
iTags = lens _iTags (\s a -> s {_iTags = a}) . _Default . _Map

-- | The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
iRoleARN :: Lens' Input (Maybe Text)
iRoleARN = lens _iRoleARN (\s a -> s {_iRoleARN = a})

instance FromJSON Input where
  parseJSON =
    withObject
      "Input"
      ( \x ->
          Input'
            <$> (x .:? "state")
            <*> (x .:? "securityGroups" .!= mempty)
            <*> (x .:? "arn")
            <*> (x .:? "inputDevices" .!= mempty)
            <*> (x .:? "sources" .!= mempty)
            <*> (x .:? "destinations" .!= mempty)
            <*> (x .:? "name")
            <*> (x .:? "attachedChannels" .!= mempty)
            <*> (x .:? "id")
            <*> (x .:? "inputClass")
            <*> (x .:? "type")
            <*> (x .:? "mediaConnectFlows" .!= mempty)
            <*> (x .:? "inputSourceType")
            <*> (x .:? "tags" .!= mempty)
            <*> (x .:? "roleArn")
      )

instance Hashable Input

instance NFData Input
