{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ChannelSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ChannelSpecification where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.CompressionType
import Network.AWS.SageMaker.Types.TrainingInputMode

-- | Defines a named input source, called a channel, to be used by an algorithm.
--
--
--
-- /See:/ 'channelSpecification' smart constructor.
data ChannelSpecification = ChannelSpecification'
  { _csSupportedCompressionTypes ::
      !(Maybe [CompressionType]),
    _csIsRequired :: !(Maybe Bool),
    _csDescription :: !(Maybe Text),
    _csName :: !Text,
    _csSupportedContentTypes :: ![Text],
    _csSupportedInputModes ::
      !(List1 TrainingInputMode)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ChannelSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csSupportedCompressionTypes' - The allowed compression types, if data compression is used.
--
-- * 'csIsRequired' - Indicates whether the channel is required by the algorithm.
--
-- * 'csDescription' - A brief description of the channel.
--
-- * 'csName' - The name of the channel.
--
-- * 'csSupportedContentTypes' - The supported MIME types for the data.
--
-- * 'csSupportedInputModes' - The allowed input mode, either FILE or PIPE. In FILE mode, Amazon SageMaker copies the data from the input source onto the local Amazon Elastic Block Store (Amazon EBS) volumes before starting your training algorithm. This is the most commonly used input mode. In PIPE mode, Amazon SageMaker streams input data from the source directly to your algorithm without using the EBS volume.
channelSpecification ::
  -- | 'csName'
  Text ->
  -- | 'csSupportedInputModes'
  NonEmpty TrainingInputMode ->
  ChannelSpecification
channelSpecification pName_ pSupportedInputModes_ =
  ChannelSpecification'
    { _csSupportedCompressionTypes = Nothing,
      _csIsRequired = Nothing,
      _csDescription = Nothing,
      _csName = pName_,
      _csSupportedContentTypes = mempty,
      _csSupportedInputModes = _List1 # pSupportedInputModes_
    }

-- | The allowed compression types, if data compression is used.
csSupportedCompressionTypes :: Lens' ChannelSpecification [CompressionType]
csSupportedCompressionTypes = lens _csSupportedCompressionTypes (\s a -> s {_csSupportedCompressionTypes = a}) . _Default . _Coerce

-- | Indicates whether the channel is required by the algorithm.
csIsRequired :: Lens' ChannelSpecification (Maybe Bool)
csIsRequired = lens _csIsRequired (\s a -> s {_csIsRequired = a})

-- | A brief description of the channel.
csDescription :: Lens' ChannelSpecification (Maybe Text)
csDescription = lens _csDescription (\s a -> s {_csDescription = a})

-- | The name of the channel.
csName :: Lens' ChannelSpecification Text
csName = lens _csName (\s a -> s {_csName = a})

-- | The supported MIME types for the data.
csSupportedContentTypes :: Lens' ChannelSpecification [Text]
csSupportedContentTypes = lens _csSupportedContentTypes (\s a -> s {_csSupportedContentTypes = a}) . _Coerce

-- | The allowed input mode, either FILE or PIPE. In FILE mode, Amazon SageMaker copies the data from the input source onto the local Amazon Elastic Block Store (Amazon EBS) volumes before starting your training algorithm. This is the most commonly used input mode. In PIPE mode, Amazon SageMaker streams input data from the source directly to your algorithm without using the EBS volume.
csSupportedInputModes :: Lens' ChannelSpecification (NonEmpty TrainingInputMode)
csSupportedInputModes = lens _csSupportedInputModes (\s a -> s {_csSupportedInputModes = a}) . _List1

instance FromJSON ChannelSpecification where
  parseJSON =
    withObject
      "ChannelSpecification"
      ( \x ->
          ChannelSpecification'
            <$> (x .:? "SupportedCompressionTypes" .!= mempty)
            <*> (x .:? "IsRequired")
            <*> (x .:? "Description")
            <*> (x .: "Name")
            <*> (x .:? "SupportedContentTypes" .!= mempty)
            <*> (x .: "SupportedInputModes")
      )

instance Hashable ChannelSpecification

instance NFData ChannelSpecification

instance ToJSON ChannelSpecification where
  toJSON ChannelSpecification' {..} =
    object
      ( catMaybes
          [ ("SupportedCompressionTypes" .=) <$> _csSupportedCompressionTypes,
            ("IsRequired" .=) <$> _csIsRequired,
            ("Description" .=) <$> _csDescription,
            Just ("Name" .= _csName),
            Just ("SupportedContentTypes" .= _csSupportedContentTypes),
            Just ("SupportedInputModes" .= _csSupportedInputModes)
          ]
      )
