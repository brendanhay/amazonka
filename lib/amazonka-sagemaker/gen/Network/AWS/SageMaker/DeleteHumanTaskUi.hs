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
-- Module      : Network.AWS.SageMaker.DeleteHumanTaskUi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to delete a human task user interface (worker task template).
--
--
-- To see a list of human task user interfaces (work task templates) in your account, use . When you delete a worker task template, it no longer appears when you call @ListHumanTaskUis@ .
module Network.AWS.SageMaker.DeleteHumanTaskUi
  ( -- * Creating a Request
    deleteHumanTaskUi,
    DeleteHumanTaskUi,

    -- * Request Lenses
    dhtuHumanTaskUiName,

    -- * Destructuring the Response
    deleteHumanTaskUiResponse,
    DeleteHumanTaskUiResponse,

    -- * Response Lenses
    dhtursResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'deleteHumanTaskUi' smart constructor.
newtype DeleteHumanTaskUi = DeleteHumanTaskUi'
  { _dhtuHumanTaskUiName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteHumanTaskUi' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhtuHumanTaskUiName' - The name of the human task user interface (work task template) you want to delete.
deleteHumanTaskUi ::
  -- | 'dhtuHumanTaskUiName'
  Text ->
  DeleteHumanTaskUi
deleteHumanTaskUi pHumanTaskUiName_ =
  DeleteHumanTaskUi' {_dhtuHumanTaskUiName = pHumanTaskUiName_}

-- | The name of the human task user interface (work task template) you want to delete.
dhtuHumanTaskUiName :: Lens' DeleteHumanTaskUi Text
dhtuHumanTaskUiName = lens _dhtuHumanTaskUiName (\s a -> s {_dhtuHumanTaskUiName = a})

instance AWSRequest DeleteHumanTaskUi where
  type Rs DeleteHumanTaskUi = DeleteHumanTaskUiResponse
  request = postJSON sageMaker
  response =
    receiveEmpty
      (\s h x -> DeleteHumanTaskUiResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteHumanTaskUi

instance NFData DeleteHumanTaskUi

instance ToHeaders DeleteHumanTaskUi where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.DeleteHumanTaskUi" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteHumanTaskUi where
  toJSON DeleteHumanTaskUi' {..} =
    object
      (catMaybes [Just ("HumanTaskUiName" .= _dhtuHumanTaskUiName)])

instance ToPath DeleteHumanTaskUi where
  toPath = const "/"

instance ToQuery DeleteHumanTaskUi where
  toQuery = const mempty

-- | /See:/ 'deleteHumanTaskUiResponse' smart constructor.
newtype DeleteHumanTaskUiResponse = DeleteHumanTaskUiResponse'
  { _dhtursResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteHumanTaskUiResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhtursResponseStatus' - -- | The response status code.
deleteHumanTaskUiResponse ::
  -- | 'dhtursResponseStatus'
  Int ->
  DeleteHumanTaskUiResponse
deleteHumanTaskUiResponse pResponseStatus_ =
  DeleteHumanTaskUiResponse'
    { _dhtursResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dhtursResponseStatus :: Lens' DeleteHumanTaskUiResponse Int
dhtursResponseStatus = lens _dhtursResponseStatus (\s a -> s {_dhtursResponseStatus = a})

instance NFData DeleteHumanTaskUiResponse
