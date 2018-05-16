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
-- Module      : Network.AWS.LexModels.GetBotChannelAssociation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the association between an Amazon Lex bot and a messaging platform.
--
--
-- This operation requires permissions for the @lex:GetBotChannelAssociation@ action.
--
module Network.AWS.LexModels.GetBotChannelAssociation
    (
    -- * Creating a Request
      getBotChannelAssociation
    , GetBotChannelAssociation
    -- * Request Lenses
    , gName
    , gBotName
    , gBotAlias

    -- * Destructuring the Response
    , getBotChannelAssociationResponse
    , GetBotChannelAssociationResponse
    -- * Response Lenses
    , gbcarsFailureReason
    , gbcarsStatus
    , gbcarsBotAlias
    , gbcarsBotName
    , gbcarsBotConfiguration
    , gbcarsCreatedDate
    , gbcarsName
    , gbcarsType
    , gbcarsDescription
    , gbcarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getBotChannelAssociation' smart constructor.
data GetBotChannelAssociation = GetBotChannelAssociation'
  { _gName     :: !Text
  , _gBotName  :: !Text
  , _gBotAlias :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBotChannelAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gName' - The name of the association between the bot and the channel. The name is case sensitive.
--
-- * 'gBotName' - The name of the Amazon Lex bot.
--
-- * 'gBotAlias' - An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
getBotChannelAssociation
    :: Text -- ^ 'gName'
    -> Text -- ^ 'gBotName'
    -> Text -- ^ 'gBotAlias'
    -> GetBotChannelAssociation
getBotChannelAssociation pName_ pBotName_ pBotAlias_ =
  GetBotChannelAssociation'
    {_gName = pName_, _gBotName = pBotName_, _gBotAlias = pBotAlias_}


-- | The name of the association between the bot and the channel. The name is case sensitive.
gName :: Lens' GetBotChannelAssociation Text
gName = lens _gName (\ s a -> s{_gName = a})

-- | The name of the Amazon Lex bot.
gBotName :: Lens' GetBotChannelAssociation Text
gBotName = lens _gBotName (\ s a -> s{_gBotName = a})

-- | An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
gBotAlias :: Lens' GetBotChannelAssociation Text
gBotAlias = lens _gBotAlias (\ s a -> s{_gBotAlias = a})

instance AWSRequest GetBotChannelAssociation where
        type Rs GetBotChannelAssociation =
             GetBotChannelAssociationResponse
        request = get lexModels
        response
          = receiveJSON
              (\ s h x ->
                 GetBotChannelAssociationResponse' <$>
                   (x .?> "failureReason") <*> (x .?> "status") <*>
                     (x .?> "botAlias")
                     <*> (x .?> "botName")
                     <*> (x .?> "botConfiguration" .!@ mempty)
                     <*> (x .?> "createdDate")
                     <*> (x .?> "name")
                     <*> (x .?> "type")
                     <*> (x .?> "description")
                     <*> (pure (fromEnum s)))

instance Hashable GetBotChannelAssociation where

instance NFData GetBotChannelAssociation where

instance ToHeaders GetBotChannelAssociation where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetBotChannelAssociation where
        toPath GetBotChannelAssociation'{..}
          = mconcat
              ["/bots/", toBS _gBotName, "/aliases/",
               toBS _gBotAlias, "/channels/", toBS _gName]

instance ToQuery GetBotChannelAssociation where
        toQuery = const mempty

-- | /See:/ 'getBotChannelAssociationResponse' smart constructor.
data GetBotChannelAssociationResponse = GetBotChannelAssociationResponse'
  { _gbcarsFailureReason    :: !(Maybe Text)
  , _gbcarsStatus           :: !(Maybe ChannelStatus)
  , _gbcarsBotAlias         :: !(Maybe Text)
  , _gbcarsBotName          :: !(Maybe Text)
  , _gbcarsBotConfiguration :: !(Maybe (Sensitive (Map Text Text)))
  , _gbcarsCreatedDate      :: !(Maybe POSIX)
  , _gbcarsName             :: !(Maybe Text)
  , _gbcarsType             :: !(Maybe ChannelType)
  , _gbcarsDescription      :: !(Maybe Text)
  , _gbcarsResponseStatus   :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBotChannelAssociationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbcarsFailureReason' - If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to create the association.
--
-- * 'gbcarsStatus' - The status of the bot channel.      * @CREATED@ - The channel has been created and is ready for use.     * @IN_PROGRESS@ - Channel creation is in progress.     * @FAILED@ - There was an error creating the channel. For information about the reason for the failure, see the @failureReason@ field.
--
-- * 'gbcarsBotAlias' - An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
--
-- * 'gbcarsBotName' - The name of the Amazon Lex bot.
--
-- * 'gbcarsBotConfiguration' - Provides information that the messaging platform needs to communicate with the Amazon Lex bot.
--
-- * 'gbcarsCreatedDate' - The date that the association between the bot and the channel was created.
--
-- * 'gbcarsName' - The name of the association between the bot and the channel.
--
-- * 'gbcarsType' - The type of the messaging platform.
--
-- * 'gbcarsDescription' - A description of the association between the bot and the channel.
--
-- * 'gbcarsResponseStatus' - -- | The response status code.
getBotChannelAssociationResponse
    :: Int -- ^ 'gbcarsResponseStatus'
    -> GetBotChannelAssociationResponse
getBotChannelAssociationResponse pResponseStatus_ =
  GetBotChannelAssociationResponse'
    { _gbcarsFailureReason = Nothing
    , _gbcarsStatus = Nothing
    , _gbcarsBotAlias = Nothing
    , _gbcarsBotName = Nothing
    , _gbcarsBotConfiguration = Nothing
    , _gbcarsCreatedDate = Nothing
    , _gbcarsName = Nothing
    , _gbcarsType = Nothing
    , _gbcarsDescription = Nothing
    , _gbcarsResponseStatus = pResponseStatus_
    }


-- | If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to create the association.
gbcarsFailureReason :: Lens' GetBotChannelAssociationResponse (Maybe Text)
gbcarsFailureReason = lens _gbcarsFailureReason (\ s a -> s{_gbcarsFailureReason = a})

-- | The status of the bot channel.      * @CREATED@ - The channel has been created and is ready for use.     * @IN_PROGRESS@ - Channel creation is in progress.     * @FAILED@ - There was an error creating the channel. For information about the reason for the failure, see the @failureReason@ field.
gbcarsStatus :: Lens' GetBotChannelAssociationResponse (Maybe ChannelStatus)
gbcarsStatus = lens _gbcarsStatus (\ s a -> s{_gbcarsStatus = a})

-- | An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
gbcarsBotAlias :: Lens' GetBotChannelAssociationResponse (Maybe Text)
gbcarsBotAlias = lens _gbcarsBotAlias (\ s a -> s{_gbcarsBotAlias = a})

-- | The name of the Amazon Lex bot.
gbcarsBotName :: Lens' GetBotChannelAssociationResponse (Maybe Text)
gbcarsBotName = lens _gbcarsBotName (\ s a -> s{_gbcarsBotName = a})

-- | Provides information that the messaging platform needs to communicate with the Amazon Lex bot.
gbcarsBotConfiguration :: Lens' GetBotChannelAssociationResponse (Maybe (HashMap Text Text))
gbcarsBotConfiguration = lens _gbcarsBotConfiguration (\ s a -> s{_gbcarsBotConfiguration = a}) . mapping (_Sensitive . _Map)

-- | The date that the association between the bot and the channel was created.
gbcarsCreatedDate :: Lens' GetBotChannelAssociationResponse (Maybe UTCTime)
gbcarsCreatedDate = lens _gbcarsCreatedDate (\ s a -> s{_gbcarsCreatedDate = a}) . mapping _Time

-- | The name of the association between the bot and the channel.
gbcarsName :: Lens' GetBotChannelAssociationResponse (Maybe Text)
gbcarsName = lens _gbcarsName (\ s a -> s{_gbcarsName = a})

-- | The type of the messaging platform.
gbcarsType :: Lens' GetBotChannelAssociationResponse (Maybe ChannelType)
gbcarsType = lens _gbcarsType (\ s a -> s{_gbcarsType = a})

-- | A description of the association between the bot and the channel.
gbcarsDescription :: Lens' GetBotChannelAssociationResponse (Maybe Text)
gbcarsDescription = lens _gbcarsDescription (\ s a -> s{_gbcarsDescription = a})

-- | -- | The response status code.
gbcarsResponseStatus :: Lens' GetBotChannelAssociationResponse Int
gbcarsResponseStatus = lens _gbcarsResponseStatus (\ s a -> s{_gbcarsResponseStatus = a})

instance NFData GetBotChannelAssociationResponse
         where
