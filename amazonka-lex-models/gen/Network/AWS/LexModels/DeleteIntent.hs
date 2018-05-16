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
-- Module      : Network.AWS.LexModels.DeleteIntent
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all versions of the intent, including the @> LATEST@ version. To delete a specific version of the intent, use the 'DeleteIntentVersion' operation.
--
--
-- You can delete a version of an intent only if it is not referenced. To delete an intent that is referred to in one or more bots (see 'how-it-works' ), you must remove those references first.
--
-- This operation requires permission for the @lex:DeleteIntent@ action.
--
module Network.AWS.LexModels.DeleteIntent
    (
    -- * Creating a Request
      deleteIntent
    , DeleteIntent
    -- * Request Lenses
    , diName

    -- * Destructuring the Response
    , deleteIntentResponse
    , DeleteIntentResponse
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteIntent' smart constructor.
newtype DeleteIntent = DeleteIntent'
  { _diName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteIntent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diName' - The name of the intent. The name is case sensitive.
deleteIntent
    :: Text -- ^ 'diName'
    -> DeleteIntent
deleteIntent pName_ = DeleteIntent' {_diName = pName_}


-- | The name of the intent. The name is case sensitive.
diName :: Lens' DeleteIntent Text
diName = lens _diName (\ s a -> s{_diName = a})

instance AWSRequest DeleteIntent where
        type Rs DeleteIntent = DeleteIntentResponse
        request = delete lexModels
        response = receiveNull DeleteIntentResponse'

instance Hashable DeleteIntent where

instance NFData DeleteIntent where

instance ToHeaders DeleteIntent where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteIntent where
        toPath DeleteIntent'{..}
          = mconcat ["/intents/", toBS _diName]

instance ToQuery DeleteIntent where
        toQuery = const mempty

-- | /See:/ 'deleteIntentResponse' smart constructor.
data DeleteIntentResponse =
  DeleteIntentResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteIntentResponse' with the minimum fields required to make a request.
--
deleteIntentResponse
    :: DeleteIntentResponse
deleteIntentResponse = DeleteIntentResponse'


instance NFData DeleteIntentResponse where
