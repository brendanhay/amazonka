{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteModelPackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a model package.
--
--
-- A model package is used to create Amazon SageMaker models or list on AWS Marketplace. Buyers can subscribe to model packages listed on AWS Marketplace to create models in Amazon SageMaker.
module Network.AWS.SageMaker.DeleteModelPackage
  ( -- * Creating a Request
    deleteModelPackage,
    DeleteModelPackage,

    -- * Request Lenses
    dmpModelPackageName,

    -- * Destructuring the Response
    deleteModelPackageResponse,
    DeleteModelPackageResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'deleteModelPackage' smart constructor.
newtype DeleteModelPackage = DeleteModelPackage'
  { _dmpModelPackageName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteModelPackage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmpModelPackageName' - The name of the model package. The name must have 1 to 63 characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
deleteModelPackage ::
  -- | 'dmpModelPackageName'
  Text ->
  DeleteModelPackage
deleteModelPackage pModelPackageName_ =
  DeleteModelPackage' {_dmpModelPackageName = pModelPackageName_}

-- | The name of the model package. The name must have 1 to 63 characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
dmpModelPackageName :: Lens' DeleteModelPackage Text
dmpModelPackageName = lens _dmpModelPackageName (\s a -> s {_dmpModelPackageName = a})

instance AWSRequest DeleteModelPackage where
  type Rs DeleteModelPackage = DeleteModelPackageResponse
  request = postJSON sageMaker
  response = receiveNull DeleteModelPackageResponse'

instance Hashable DeleteModelPackage

instance NFData DeleteModelPackage

instance ToHeaders DeleteModelPackage where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.DeleteModelPackage" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteModelPackage where
  toJSON DeleteModelPackage' {..} =
    object
      (catMaybes [Just ("ModelPackageName" .= _dmpModelPackageName)])

instance ToPath DeleteModelPackage where
  toPath = const "/"

instance ToQuery DeleteModelPackage where
  toQuery = const mempty

-- | /See:/ 'deleteModelPackageResponse' smart constructor.
data DeleteModelPackageResponse = DeleteModelPackageResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteModelPackageResponse' with the minimum fields required to make a request.
deleteModelPackageResponse ::
  DeleteModelPackageResponse
deleteModelPackageResponse = DeleteModelPackageResponse'

instance NFData DeleteModelPackageResponse
