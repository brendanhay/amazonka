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
-- Module      : Network.AWS.LexModels.DeleteBotVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific version of a bot. To delete all versions of a bot, use the 'DeleteBot' operation.
--
--
-- This operation requires permissions for the @lex:DeleteBotVersion@ action.
--
module Network.AWS.LexModels.DeleteBotVersion
    (
    -- * Creating a Request
      deleteBotVersion
    , DeleteBotVersion
    -- * Request Lenses
    , dbvName
    , dbvVersion

    -- * Destructuring the Response
    , deleteBotVersionResponse
    , DeleteBotVersionResponse
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteBotVersion' smart constructor.
data DeleteBotVersion = DeleteBotVersion'
  { _dbvName    :: !Text
  , _dbvVersion :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBotVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbvName' - The name of the bot.
--
-- * 'dbvVersion' - The version of the bot to delete. You cannot delete the @> LATEST@ version of the bot. To delete the @> LATEST@ version, use the 'DeleteBot' operation.
deleteBotVersion
    :: Text -- ^ 'dbvName'
    -> Text -- ^ 'dbvVersion'
    -> DeleteBotVersion
deleteBotVersion pName_ pVersion_ =
  DeleteBotVersion' {_dbvName = pName_, _dbvVersion = pVersion_}


-- | The name of the bot.
dbvName :: Lens' DeleteBotVersion Text
dbvName = lens _dbvName (\ s a -> s{_dbvName = a})

-- | The version of the bot to delete. You cannot delete the @> LATEST@ version of the bot. To delete the @> LATEST@ version, use the 'DeleteBot' operation.
dbvVersion :: Lens' DeleteBotVersion Text
dbvVersion = lens _dbvVersion (\ s a -> s{_dbvVersion = a})

instance AWSRequest DeleteBotVersion where
        type Rs DeleteBotVersion = DeleteBotVersionResponse
        request = delete lexModels
        response = receiveNull DeleteBotVersionResponse'

instance Hashable DeleteBotVersion where

instance NFData DeleteBotVersion where

instance ToHeaders DeleteBotVersion where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteBotVersion where
        toPath DeleteBotVersion'{..}
          = mconcat
              ["/bots/", toBS _dbvName, "/versions/",
               toBS _dbvVersion]

instance ToQuery DeleteBotVersion where
        toQuery = const mempty

-- | /See:/ 'deleteBotVersionResponse' smart constructor.
data DeleteBotVersionResponse =
  DeleteBotVersionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBotVersionResponse' with the minimum fields required to make a request.
--
deleteBotVersionResponse
    :: DeleteBotVersionResponse
deleteBotVersionResponse = DeleteBotVersionResponse'


instance NFData DeleteBotVersionResponse where
