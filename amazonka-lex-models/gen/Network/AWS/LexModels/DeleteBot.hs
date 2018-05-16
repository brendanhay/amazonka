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
-- Module      : Network.AWS.LexModels.DeleteBot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all versions of the bot, including the @> LATEST@ version. To delete a specific version of the bot, use the 'DeleteBotVersion' operation.
--
--
-- If a bot has an alias, you can't delete it. Instead, the @DeleteBot@ operation returns a @ResourceInUseException@ exception that includes a reference to the alias that refers to the bot. To remove the reference to the bot, delete the alias. If you get the same exception again, delete the referring alias until the @DeleteBot@ operation is successful.
--
-- This operation requires permissions for the @lex:DeleteBot@ action.
--
module Network.AWS.LexModels.DeleteBot
    (
    -- * Creating a Request
      deleteBot
    , DeleteBot
    -- * Request Lenses
    , dbName

    -- * Destructuring the Response
    , deleteBotResponse
    , DeleteBotResponse
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteBot' smart constructor.
newtype DeleteBot = DeleteBot'
  { _dbName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbName' - The name of the bot. The name is case sensitive.
deleteBot
    :: Text -- ^ 'dbName'
    -> DeleteBot
deleteBot pName_ = DeleteBot' {_dbName = pName_}


-- | The name of the bot. The name is case sensitive.
dbName :: Lens' DeleteBot Text
dbName = lens _dbName (\ s a -> s{_dbName = a})

instance AWSRequest DeleteBot where
        type Rs DeleteBot = DeleteBotResponse
        request = delete lexModels
        response = receiveNull DeleteBotResponse'

instance Hashable DeleteBot where

instance NFData DeleteBot where

instance ToHeaders DeleteBot where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteBot where
        toPath DeleteBot'{..}
          = mconcat ["/bots/", toBS _dbName]

instance ToQuery DeleteBot where
        toQuery = const mempty

-- | /See:/ 'deleteBotResponse' smart constructor.
data DeleteBotResponse =
  DeleteBotResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBotResponse' with the minimum fields required to make a request.
--
deleteBotResponse
    :: DeleteBotResponse
deleteBotResponse = DeleteBotResponse'


instance NFData DeleteBotResponse where
