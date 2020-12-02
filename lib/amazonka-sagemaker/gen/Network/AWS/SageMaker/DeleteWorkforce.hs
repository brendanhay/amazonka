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
-- Module      : Network.AWS.SageMaker.DeleteWorkforce
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to delete a workforce.
--
--
-- If you want to create a new workforce in an AWS Region where a workforce already exists, use this operation to delete the existing workforce and then use to create a new workforce.
--
-- /Important:/ If a private workforce contains one or more work teams, you must use the operation to delete all work teams before you delete the workforce. If you try to delete a workforce that contains one or more work teams, you will recieve a @ResourceInUse@ error.
module Network.AWS.SageMaker.DeleteWorkforce
  ( -- * Creating a Request
    deleteWorkforce,
    DeleteWorkforce,

    -- * Request Lenses
    dwWorkforceName,

    -- * Destructuring the Response
    deleteWorkforceResponse,
    DeleteWorkforceResponse,

    -- * Response Lenses
    deleteworkforceersResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'deleteWorkforce' smart constructor.
newtype DeleteWorkforce = DeleteWorkforce'
  { _dwWorkforceName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteWorkforce' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwWorkforceName' - The name of the workforce.
deleteWorkforce ::
  -- | 'dwWorkforceName'
  Text ->
  DeleteWorkforce
deleteWorkforce pWorkforceName_ =
  DeleteWorkforce' {_dwWorkforceName = pWorkforceName_}

-- | The name of the workforce.
dwWorkforceName :: Lens' DeleteWorkforce Text
dwWorkforceName = lens _dwWorkforceName (\s a -> s {_dwWorkforceName = a})

instance AWSRequest DeleteWorkforce where
  type Rs DeleteWorkforce = DeleteWorkforceResponse
  request = postJSON sageMaker
  response =
    receiveEmpty
      (\s h x -> DeleteWorkforceResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteWorkforce

instance NFData DeleteWorkforce

instance ToHeaders DeleteWorkforce where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.DeleteWorkforce" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteWorkforce where
  toJSON DeleteWorkforce' {..} =
    object (catMaybes [Just ("WorkforceName" .= _dwWorkforceName)])

instance ToPath DeleteWorkforce where
  toPath = const "/"

instance ToQuery DeleteWorkforce where
  toQuery = const mempty

-- | /See:/ 'deleteWorkforceResponse' smart constructor.
newtype DeleteWorkforceResponse = DeleteWorkforceResponse'
  { _deleteworkforceersResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteWorkforceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deleteworkforceersResponseStatus' - -- | The response status code.
deleteWorkforceResponse ::
  -- | 'deleteworkforceersResponseStatus'
  Int ->
  DeleteWorkforceResponse
deleteWorkforceResponse pResponseStatus_ =
  DeleteWorkforceResponse'
    { _deleteworkforceersResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
deleteworkforceersResponseStatus :: Lens' DeleteWorkforceResponse Int
deleteworkforceersResponseStatus = lens _deleteworkforceersResponseStatus (\s a -> s {_deleteworkforceersResponseStatus = a})

instance NFData DeleteWorkforceResponse
