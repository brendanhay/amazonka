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
-- Module      : Network.AWS.LexModels.DeleteSlotType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all versions of the slot type, including the @> LATEST@ version. To delete a specific version of the slot type, use the 'DeleteSlotTypeVersion' operation.
--
--
-- You can delete a version of a slot type only if it is not referenced. To delete a slot type that is referred to in one or more intents, you must remove those references first.
--
-- This operation requires permission for the @lex:DeleteSlotType@ action.
--
module Network.AWS.LexModels.DeleteSlotType
    (
    -- * Creating a Request
      deleteSlotType
    , DeleteSlotType
    -- * Request Lenses
    , dstName

    -- * Destructuring the Response
    , deleteSlotTypeResponse
    , DeleteSlotTypeResponse
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSlotType' smart constructor.
newtype DeleteSlotType = DeleteSlotType'
  { _dstName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSlotType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dstName' - The name of the slot type. The name is case sensitive.
deleteSlotType
    :: Text -- ^ 'dstName'
    -> DeleteSlotType
deleteSlotType pName_ = DeleteSlotType' {_dstName = pName_}


-- | The name of the slot type. The name is case sensitive.
dstName :: Lens' DeleteSlotType Text
dstName = lens _dstName (\ s a -> s{_dstName = a})

instance AWSRequest DeleteSlotType where
        type Rs DeleteSlotType = DeleteSlotTypeResponse
        request = delete lexModels
        response = receiveNull DeleteSlotTypeResponse'

instance Hashable DeleteSlotType where

instance NFData DeleteSlotType where

instance ToHeaders DeleteSlotType where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteSlotType where
        toPath DeleteSlotType'{..}
          = mconcat ["/slottypes/", toBS _dstName]

instance ToQuery DeleteSlotType where
        toQuery = const mempty

-- | /See:/ 'deleteSlotTypeResponse' smart constructor.
data DeleteSlotTypeResponse =
  DeleteSlotTypeResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSlotTypeResponse' with the minimum fields required to make a request.
--
deleteSlotTypeResponse
    :: DeleteSlotTypeResponse
deleteSlotTypeResponse = DeleteSlotTypeResponse'


instance NFData DeleteSlotTypeResponse where
