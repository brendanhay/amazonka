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
-- Module      : Network.AWS.KMS.DeleteImportedKeyMaterial
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes key material that you previously imported. This operation makes the specified customer master key (CMK) unusable. For more information about importing key material into AWS KMS, see <http://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html Importing Key Material> in the /AWS Key Management Service Developer Guide/ . You cannot perform this operation on a CMK in a different AWS account.
--
--
-- When the specified CMK is in the @PendingDeletion@ state, this operation does not change the CMK's state. Otherwise, it changes the CMK's state to @PendingImport@ .
--
-- After you delete key material, you can use 'ImportKeyMaterial' to reimport the same key material into the CMK.
--
module Network.AWS.KMS.DeleteImportedKeyMaterial
    (
    -- * Creating a Request
      deleteImportedKeyMaterial
    , DeleteImportedKeyMaterial
    -- * Request Lenses
    , dikmKeyId

    -- * Destructuring the Response
    , deleteImportedKeyMaterialResponse
    , DeleteImportedKeyMaterialResponse
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteImportedKeyMaterial' smart constructor.
newtype DeleteImportedKeyMaterial = DeleteImportedKeyMaterial'
  { _dikmKeyId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteImportedKeyMaterial' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dikmKeyId' - The identifier of the CMK whose key material to delete. The CMK's @Origin@ must be @EXTERNAL@ . Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
deleteImportedKeyMaterial
    :: Text -- ^ 'dikmKeyId'
    -> DeleteImportedKeyMaterial
deleteImportedKeyMaterial pKeyId_ =
  DeleteImportedKeyMaterial' {_dikmKeyId = pKeyId_}


-- | The identifier of the CMK whose key material to delete. The CMK's @Origin@ must be @EXTERNAL@ . Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
dikmKeyId :: Lens' DeleteImportedKeyMaterial Text
dikmKeyId = lens _dikmKeyId (\ s a -> s{_dikmKeyId = a})

instance AWSRequest DeleteImportedKeyMaterial where
        type Rs DeleteImportedKeyMaterial =
             DeleteImportedKeyMaterialResponse
        request = postJSON kms
        response
          = receiveNull DeleteImportedKeyMaterialResponse'

instance Hashable DeleteImportedKeyMaterial where

instance NFData DeleteImportedKeyMaterial where

instance ToHeaders DeleteImportedKeyMaterial where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.DeleteImportedKeyMaterial" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteImportedKeyMaterial where
        toJSON DeleteImportedKeyMaterial'{..}
          = object (catMaybes [Just ("KeyId" .= _dikmKeyId)])

instance ToPath DeleteImportedKeyMaterial where
        toPath = const "/"

instance ToQuery DeleteImportedKeyMaterial where
        toQuery = const mempty

-- | /See:/ 'deleteImportedKeyMaterialResponse' smart constructor.
data DeleteImportedKeyMaterialResponse =
  DeleteImportedKeyMaterialResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteImportedKeyMaterialResponse' with the minimum fields required to make a request.
--
deleteImportedKeyMaterialResponse
    :: DeleteImportedKeyMaterialResponse
deleteImportedKeyMaterialResponse = DeleteImportedKeyMaterialResponse'


instance NFData DeleteImportedKeyMaterialResponse
         where
