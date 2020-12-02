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
-- Module      : Network.AWS.Translate.DeleteTerminology
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A synchronous action that deletes a custom terminology.
module Network.AWS.Translate.DeleteTerminology
  ( -- * Creating a Request
    deleteTerminology,
    DeleteTerminology,

    -- * Request Lenses
    dtName,

    -- * Destructuring the Response
    deleteTerminologyResponse,
    DeleteTerminologyResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Translate.Types

-- | /See:/ 'deleteTerminology' smart constructor.
newtype DeleteTerminology = DeleteTerminology' {_dtName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteTerminology' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtName' - The name of the custom terminology being deleted.
deleteTerminology ::
  -- | 'dtName'
  Text ->
  DeleteTerminology
deleteTerminology pName_ = DeleteTerminology' {_dtName = pName_}

-- | The name of the custom terminology being deleted.
dtName :: Lens' DeleteTerminology Text
dtName = lens _dtName (\s a -> s {_dtName = a})

instance AWSRequest DeleteTerminology where
  type Rs DeleteTerminology = DeleteTerminologyResponse
  request = postJSON translate
  response = receiveNull DeleteTerminologyResponse'

instance Hashable DeleteTerminology

instance NFData DeleteTerminology

instance ToHeaders DeleteTerminology where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSShineFrontendService_20170701.DeleteTerminology" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteTerminology where
  toJSON DeleteTerminology' {..} =
    object (catMaybes [Just ("Name" .= _dtName)])

instance ToPath DeleteTerminology where
  toPath = const "/"

instance ToQuery DeleteTerminology where
  toQuery = const mempty

-- | /See:/ 'deleteTerminologyResponse' smart constructor.
data DeleteTerminologyResponse = DeleteTerminologyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteTerminologyResponse' with the minimum fields required to make a request.
deleteTerminologyResponse ::
  DeleteTerminologyResponse
deleteTerminologyResponse = DeleteTerminologyResponse'

instance NFData DeleteTerminologyResponse
