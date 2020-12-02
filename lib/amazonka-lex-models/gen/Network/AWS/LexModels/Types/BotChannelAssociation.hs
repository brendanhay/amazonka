{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.BotChannelAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.BotChannelAssociation where

import Network.AWS.Lens
import Network.AWS.LexModels.Types.ChannelStatus
import Network.AWS.LexModels.Types.ChannelType
import Network.AWS.Prelude

-- | Represents an association between an Amazon Lex bot and an external messaging platform.
--
--
--
-- /See:/ 'botChannelAssociation' smart constructor.
data BotChannelAssociation = BotChannelAssociation'
  { _bcaFailureReason ::
      !(Maybe Text),
    _bcaStatus :: !(Maybe ChannelStatus),
    _bcaBotAlias :: !(Maybe Text),
    _bcaBotName :: !(Maybe Text),
    _bcaBotConfiguration ::
      !(Maybe (Sensitive (Map Text (Text)))),
    _bcaCreatedDate :: !(Maybe POSIX),
    _bcaName :: !(Maybe Text),
    _bcaType :: !(Maybe ChannelType),
    _bcaDescription :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'BotChannelAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bcaFailureReason' - If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to create the association.
--
-- * 'bcaStatus' - The status of the bot channel.      * @CREATED@ - The channel has been created and is ready for use.     * @IN_PROGRESS@ - Channel creation is in progress.     * @FAILED@ - There was an error creating the channel. For information about the reason for the failure, see the @failureReason@ field.
--
-- * 'bcaBotAlias' - An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
--
-- * 'bcaBotName' - The name of the Amazon Lex bot to which this association is being made.
--
-- * 'bcaBotConfiguration' - Provides information necessary to communicate with the messaging platform.
--
-- * 'bcaCreatedDate' - The date that the association between the Amazon Lex bot and the channel was created.
--
-- * 'bcaName' - The name of the association between the bot and the channel.
--
-- * 'bcaType' - Specifies the type of association by indicating the type of channel being established between the Amazon Lex bot and the external messaging platform.
--
-- * 'bcaDescription' - A text description of the association you are creating.
botChannelAssociation ::
  BotChannelAssociation
botChannelAssociation =
  BotChannelAssociation'
    { _bcaFailureReason = Nothing,
      _bcaStatus = Nothing,
      _bcaBotAlias = Nothing,
      _bcaBotName = Nothing,
      _bcaBotConfiguration = Nothing,
      _bcaCreatedDate = Nothing,
      _bcaName = Nothing,
      _bcaType = Nothing,
      _bcaDescription = Nothing
    }

-- | If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to create the association.
bcaFailureReason :: Lens' BotChannelAssociation (Maybe Text)
bcaFailureReason = lens _bcaFailureReason (\s a -> s {_bcaFailureReason = a})

-- | The status of the bot channel.      * @CREATED@ - The channel has been created and is ready for use.     * @IN_PROGRESS@ - Channel creation is in progress.     * @FAILED@ - There was an error creating the channel. For information about the reason for the failure, see the @failureReason@ field.
bcaStatus :: Lens' BotChannelAssociation (Maybe ChannelStatus)
bcaStatus = lens _bcaStatus (\s a -> s {_bcaStatus = a})

-- | An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
bcaBotAlias :: Lens' BotChannelAssociation (Maybe Text)
bcaBotAlias = lens _bcaBotAlias (\s a -> s {_bcaBotAlias = a})

-- | The name of the Amazon Lex bot to which this association is being made.
bcaBotName :: Lens' BotChannelAssociation (Maybe Text)
bcaBotName = lens _bcaBotName (\s a -> s {_bcaBotName = a})

-- | Provides information necessary to communicate with the messaging platform.
bcaBotConfiguration :: Lens' BotChannelAssociation (Maybe (HashMap Text (Text)))
bcaBotConfiguration = lens _bcaBotConfiguration (\s a -> s {_bcaBotConfiguration = a}) . mapping (_Sensitive . _Map)

-- | The date that the association between the Amazon Lex bot and the channel was created.
bcaCreatedDate :: Lens' BotChannelAssociation (Maybe UTCTime)
bcaCreatedDate = lens _bcaCreatedDate (\s a -> s {_bcaCreatedDate = a}) . mapping _Time

-- | The name of the association between the bot and the channel.
bcaName :: Lens' BotChannelAssociation (Maybe Text)
bcaName = lens _bcaName (\s a -> s {_bcaName = a})

-- | Specifies the type of association by indicating the type of channel being established between the Amazon Lex bot and the external messaging platform.
bcaType :: Lens' BotChannelAssociation (Maybe ChannelType)
bcaType = lens _bcaType (\s a -> s {_bcaType = a})

-- | A text description of the association you are creating.
bcaDescription :: Lens' BotChannelAssociation (Maybe Text)
bcaDescription = lens _bcaDescription (\s a -> s {_bcaDescription = a})

instance FromJSON BotChannelAssociation where
  parseJSON =
    withObject
      "BotChannelAssociation"
      ( \x ->
          BotChannelAssociation'
            <$> (x .:? "failureReason")
            <*> (x .:? "status")
            <*> (x .:? "botAlias")
            <*> (x .:? "botName")
            <*> (x .:? "botConfiguration" .!= mempty)
            <*> (x .:? "createdDate")
            <*> (x .:? "name")
            <*> (x .:? "type")
            <*> (x .:? "description")
      )

instance Hashable BotChannelAssociation

instance NFData BotChannelAssociation
