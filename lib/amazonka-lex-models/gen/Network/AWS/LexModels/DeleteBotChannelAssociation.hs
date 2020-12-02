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
-- Module      : Network.AWS.LexModels.DeleteBotChannelAssociation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the association between an Amazon Lex bot and a messaging platform.
--
--
-- This operation requires permission for the @lex:DeleteBotChannelAssociation@ action.
--
module Network.AWS.LexModels.DeleteBotChannelAssociation
    (
    -- * Creating a Request
      deleteBotChannelAssociation
    , DeleteBotChannelAssociation
    -- * Request Lenses
    , dbcaName
    , dbcaBotName
    , dbcaBotAlias

    -- * Destructuring the Response
    , deleteBotChannelAssociationResponse
    , DeleteBotChannelAssociationResponse
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteBotChannelAssociation' smart constructor.
data DeleteBotChannelAssociation = DeleteBotChannelAssociation'
  { _dbcaName     :: !Text
  , _dbcaBotName  :: !Text
  , _dbcaBotAlias :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBotChannelAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbcaName' - The name of the association. The name is case sensitive.
--
-- * 'dbcaBotName' - The name of the Amazon Lex bot.
--
-- * 'dbcaBotAlias' - An alias that points to the specific version of the Amazon Lex bot to which this association is being made.
deleteBotChannelAssociation
    :: Text -- ^ 'dbcaName'
    -> Text -- ^ 'dbcaBotName'
    -> Text -- ^ 'dbcaBotAlias'
    -> DeleteBotChannelAssociation
deleteBotChannelAssociation pName_ pBotName_ pBotAlias_ =
  DeleteBotChannelAssociation'
    {_dbcaName = pName_, _dbcaBotName = pBotName_, _dbcaBotAlias = pBotAlias_}


-- | The name of the association. The name is case sensitive.
dbcaName :: Lens' DeleteBotChannelAssociation Text
dbcaName = lens _dbcaName (\ s a -> s{_dbcaName = a})

-- | The name of the Amazon Lex bot.
dbcaBotName :: Lens' DeleteBotChannelAssociation Text
dbcaBotName = lens _dbcaBotName (\ s a -> s{_dbcaBotName = a})

-- | An alias that points to the specific version of the Amazon Lex bot to which this association is being made.
dbcaBotAlias :: Lens' DeleteBotChannelAssociation Text
dbcaBotAlias = lens _dbcaBotAlias (\ s a -> s{_dbcaBotAlias = a})

instance AWSRequest DeleteBotChannelAssociation where
        type Rs DeleteBotChannelAssociation =
             DeleteBotChannelAssociationResponse
        request = delete lexModels
        response
          = receiveNull DeleteBotChannelAssociationResponse'

instance Hashable DeleteBotChannelAssociation where

instance NFData DeleteBotChannelAssociation where

instance ToHeaders DeleteBotChannelAssociation where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteBotChannelAssociation where
        toPath DeleteBotChannelAssociation'{..}
          = mconcat
              ["/bots/", toBS _dbcaBotName, "/aliases/",
               toBS _dbcaBotAlias, "/channels/", toBS _dbcaName]

instance ToQuery DeleteBotChannelAssociation where
        toQuery = const mempty

-- | /See:/ 'deleteBotChannelAssociationResponse' smart constructor.
data DeleteBotChannelAssociationResponse =
  DeleteBotChannelAssociationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBotChannelAssociationResponse' with the minimum fields required to make a request.
--
deleteBotChannelAssociationResponse
    :: DeleteBotChannelAssociationResponse
deleteBotChannelAssociationResponse = DeleteBotChannelAssociationResponse'


instance NFData DeleteBotChannelAssociationResponse
         where
