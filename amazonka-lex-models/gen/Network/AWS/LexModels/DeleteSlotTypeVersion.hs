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
-- Module      : Network.AWS.LexModels.DeleteSlotTypeVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific version of a slot type. To delete all versions of a slot type, use the 'DeleteSlotType' operation.
--
--
-- This operation requires permissions for the @lex:DeleteSlotTypeVersion@ action.
--
module Network.AWS.LexModels.DeleteSlotTypeVersion
    (
    -- * Creating a Request
      deleteSlotTypeVersion
    , DeleteSlotTypeVersion
    -- * Request Lenses
    , dstvName
    , dstvVersion

    -- * Destructuring the Response
    , deleteSlotTypeVersionResponse
    , DeleteSlotTypeVersionResponse
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSlotTypeVersion' smart constructor.
data DeleteSlotTypeVersion = DeleteSlotTypeVersion'
  { _dstvName    :: !Text
  , _dstvVersion :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSlotTypeVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dstvName' - The name of the slot type.
--
-- * 'dstvVersion' - The version of the slot type to delete. You cannot delete the @> LATEST@ version of the slot type. To delete the @> LATEST@ version, use the 'DeleteSlotType' operation.
deleteSlotTypeVersion
    :: Text -- ^ 'dstvName'
    -> Text -- ^ 'dstvVersion'
    -> DeleteSlotTypeVersion
deleteSlotTypeVersion pName_ pVersion_ =
  DeleteSlotTypeVersion' {_dstvName = pName_, _dstvVersion = pVersion_}


-- | The name of the slot type.
dstvName :: Lens' DeleteSlotTypeVersion Text
dstvName = lens _dstvName (\ s a -> s{_dstvName = a})

-- | The version of the slot type to delete. You cannot delete the @> LATEST@ version of the slot type. To delete the @> LATEST@ version, use the 'DeleteSlotType' operation.
dstvVersion :: Lens' DeleteSlotTypeVersion Text
dstvVersion = lens _dstvVersion (\ s a -> s{_dstvVersion = a})

instance AWSRequest DeleteSlotTypeVersion where
        type Rs DeleteSlotTypeVersion =
             DeleteSlotTypeVersionResponse
        request = delete lexModels
        response = receiveNull DeleteSlotTypeVersionResponse'

instance Hashable DeleteSlotTypeVersion where

instance NFData DeleteSlotTypeVersion where

instance ToHeaders DeleteSlotTypeVersion where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteSlotTypeVersion where
        toPath DeleteSlotTypeVersion'{..}
          = mconcat
              ["/slottypes/", toBS _dstvName, "/version/",
               toBS _dstvVersion]

instance ToQuery DeleteSlotTypeVersion where
        toQuery = const mempty

-- | /See:/ 'deleteSlotTypeVersionResponse' smart constructor.
data DeleteSlotTypeVersionResponse =
  DeleteSlotTypeVersionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSlotTypeVersionResponse' with the minimum fields required to make a request.
--
deleteSlotTypeVersionResponse
    :: DeleteSlotTypeVersionResponse
deleteSlotTypeVersionResponse = DeleteSlotTypeVersionResponse'


instance NFData DeleteSlotTypeVersionResponse where
