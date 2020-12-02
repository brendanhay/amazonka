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
-- Module      : Network.AWS.LexModels.DeleteIntentVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific version of an intent. To delete all versions of a intent, use the 'DeleteIntent' operation.
--
--
-- This operation requires permissions for the @lex:DeleteIntentVersion@ action.
--
module Network.AWS.LexModels.DeleteIntentVersion
    (
    -- * Creating a Request
      deleteIntentVersion
    , DeleteIntentVersion
    -- * Request Lenses
    , divName
    , divVersion

    -- * Destructuring the Response
    , deleteIntentVersionResponse
    , DeleteIntentVersionResponse
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteIntentVersion' smart constructor.
data DeleteIntentVersion = DeleteIntentVersion'
  { _divName    :: !Text
  , _divVersion :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteIntentVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'divName' - The name of the intent.
--
-- * 'divVersion' - The version of the intent to delete. You cannot delete the @> LATEST@ version of the intent. To delete the @> LATEST@ version, use the 'DeleteIntent' operation.
deleteIntentVersion
    :: Text -- ^ 'divName'
    -> Text -- ^ 'divVersion'
    -> DeleteIntentVersion
deleteIntentVersion pName_ pVersion_ =
  DeleteIntentVersion' {_divName = pName_, _divVersion = pVersion_}


-- | The name of the intent.
divName :: Lens' DeleteIntentVersion Text
divName = lens _divName (\ s a -> s{_divName = a})

-- | The version of the intent to delete. You cannot delete the @> LATEST@ version of the intent. To delete the @> LATEST@ version, use the 'DeleteIntent' operation.
divVersion :: Lens' DeleteIntentVersion Text
divVersion = lens _divVersion (\ s a -> s{_divVersion = a})

instance AWSRequest DeleteIntentVersion where
        type Rs DeleteIntentVersion =
             DeleteIntentVersionResponse
        request = delete lexModels
        response = receiveNull DeleteIntentVersionResponse'

instance Hashable DeleteIntentVersion where

instance NFData DeleteIntentVersion where

instance ToHeaders DeleteIntentVersion where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteIntentVersion where
        toPath DeleteIntentVersion'{..}
          = mconcat
              ["/intents/", toBS _divName, "/versions/",
               toBS _divVersion]

instance ToQuery DeleteIntentVersion where
        toQuery = const mempty

-- | /See:/ 'deleteIntentVersionResponse' smart constructor.
data DeleteIntentVersionResponse =
  DeleteIntentVersionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteIntentVersionResponse' with the minimum fields required to make a request.
--
deleteIntentVersionResponse
    :: DeleteIntentVersionResponse
deleteIntentVersionResponse = DeleteIntentVersionResponse'


instance NFData DeleteIntentVersionResponse where
