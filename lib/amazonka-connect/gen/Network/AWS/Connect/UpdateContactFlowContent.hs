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
-- Module      : Network.AWS.Connect.UpdateContactFlowContent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified contact flow.
--
--
-- You can also create and update contact flows using the <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language> .
module Network.AWS.Connect.UpdateContactFlowContent
  ( -- * Creating a Request
    updateContactFlowContent,
    UpdateContactFlowContent,

    -- * Request Lenses
    ucfcInstanceId,
    ucfcContactFlowId,
    ucfcContent,

    -- * Destructuring the Response
    updateContactFlowContentResponse,
    UpdateContactFlowContentResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateContactFlowContent' smart constructor.
data UpdateContactFlowContent = UpdateContactFlowContent'
  { _ucfcInstanceId ::
      !Text,
    _ucfcContactFlowId :: !Text,
    _ucfcContent :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateContactFlowContent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucfcInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'ucfcContactFlowId' - The identifier of the contact flow.
--
-- * 'ucfcContent' - The JSON string that represents contact flow’s content. For an example, see <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language-example.html Example contact flow in Amazon Connect Flow language> in the /Amazon Connect Administrator Guide/ .
updateContactFlowContent ::
  -- | 'ucfcInstanceId'
  Text ->
  -- | 'ucfcContactFlowId'
  Text ->
  -- | 'ucfcContent'
  Text ->
  UpdateContactFlowContent
updateContactFlowContent pInstanceId_ pContactFlowId_ pContent_ =
  UpdateContactFlowContent'
    { _ucfcInstanceId = pInstanceId_,
      _ucfcContactFlowId = pContactFlowId_,
      _ucfcContent = pContent_
    }

-- | The identifier of the Amazon Connect instance.
ucfcInstanceId :: Lens' UpdateContactFlowContent Text
ucfcInstanceId = lens _ucfcInstanceId (\s a -> s {_ucfcInstanceId = a})

-- | The identifier of the contact flow.
ucfcContactFlowId :: Lens' UpdateContactFlowContent Text
ucfcContactFlowId = lens _ucfcContactFlowId (\s a -> s {_ucfcContactFlowId = a})

-- | The JSON string that represents contact flow’s content. For an example, see <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language-example.html Example contact flow in Amazon Connect Flow language> in the /Amazon Connect Administrator Guide/ .
ucfcContent :: Lens' UpdateContactFlowContent Text
ucfcContent = lens _ucfcContent (\s a -> s {_ucfcContent = a})

instance AWSRequest UpdateContactFlowContent where
  type Rs UpdateContactFlowContent = UpdateContactFlowContentResponse
  request = postJSON connect
  response = receiveNull UpdateContactFlowContentResponse'

instance Hashable UpdateContactFlowContent

instance NFData UpdateContactFlowContent

instance ToHeaders UpdateContactFlowContent where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateContactFlowContent where
  toJSON UpdateContactFlowContent' {..} =
    object (catMaybes [Just ("Content" .= _ucfcContent)])

instance ToPath UpdateContactFlowContent where
  toPath UpdateContactFlowContent' {..} =
    mconcat
      [ "/contact-flows/",
        toBS _ucfcInstanceId,
        "/",
        toBS _ucfcContactFlowId,
        "/content"
      ]

instance ToQuery UpdateContactFlowContent where
  toQuery = const mempty

-- | /See:/ 'updateContactFlowContentResponse' smart constructor.
data UpdateContactFlowContentResponse = UpdateContactFlowContentResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateContactFlowContentResponse' with the minimum fields required to make a request.
updateContactFlowContentResponse ::
  UpdateContactFlowContentResponse
updateContactFlowContentResponse =
  UpdateContactFlowContentResponse'

instance NFData UpdateContactFlowContentResponse
