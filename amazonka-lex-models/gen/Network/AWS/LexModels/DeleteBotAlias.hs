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
-- Module      : Network.AWS.LexModels.DeleteBotAlias
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an alias for the specified bot.
--
--
-- You can't delete an alias that is used in the association between a bot and a messaging channel. If an alias is used in a channel association, the @DeleteBot@ operation returns a @ResourceInUseException@ exception that includes a reference to the channel association that refers to the bot. You can remove the reference to the alias by deleting the channel association. If you get the same exception again, delete the referring association until the @DeleteBotAlias@ operation is successful.
--
module Network.AWS.LexModels.DeleteBotAlias
    (
    -- * Creating a Request
      deleteBotAlias
    , DeleteBotAlias
    -- * Request Lenses
    , dbaName
    , dbaBotName

    -- * Destructuring the Response
    , deleteBotAliasResponse
    , DeleteBotAliasResponse
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteBotAlias' smart constructor.
data DeleteBotAlias = DeleteBotAlias'
  { _dbaName    :: !Text
  , _dbaBotName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBotAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbaName' - The name of the alias to delete. The name is case sensitive.
--
-- * 'dbaBotName' - The name of the bot that the alias points to.
deleteBotAlias
    :: Text -- ^ 'dbaName'
    -> Text -- ^ 'dbaBotName'
    -> DeleteBotAlias
deleteBotAlias pName_ pBotName_ =
  DeleteBotAlias' {_dbaName = pName_, _dbaBotName = pBotName_}


-- | The name of the alias to delete. The name is case sensitive.
dbaName :: Lens' DeleteBotAlias Text
dbaName = lens _dbaName (\ s a -> s{_dbaName = a})

-- | The name of the bot that the alias points to.
dbaBotName :: Lens' DeleteBotAlias Text
dbaBotName = lens _dbaBotName (\ s a -> s{_dbaBotName = a})

instance AWSRequest DeleteBotAlias where
        type Rs DeleteBotAlias = DeleteBotAliasResponse
        request = delete lexModels
        response = receiveNull DeleteBotAliasResponse'

instance Hashable DeleteBotAlias where

instance NFData DeleteBotAlias where

instance ToHeaders DeleteBotAlias where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteBotAlias where
        toPath DeleteBotAlias'{..}
          = mconcat
              ["/bots/", toBS _dbaBotName, "/aliases/",
               toBS _dbaName]

instance ToQuery DeleteBotAlias where
        toQuery = const mempty

-- | /See:/ 'deleteBotAliasResponse' smart constructor.
data DeleteBotAliasResponse =
  DeleteBotAliasResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBotAliasResponse' with the minimum fields required to make a request.
--
deleteBotAliasResponse
    :: DeleteBotAliasResponse
deleteBotAliasResponse = DeleteBotAliasResponse'


instance NFData DeleteBotAliasResponse where
