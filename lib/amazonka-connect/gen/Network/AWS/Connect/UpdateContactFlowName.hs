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
-- Module      : Network.AWS.Connect.UpdateContactFlowName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The name of the contact flow.
--
--
-- You can also create and update contact flows using the <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language> .
module Network.AWS.Connect.UpdateContactFlowName
  ( -- * Creating a Request
    updateContactFlowName,
    UpdateContactFlowName,

    -- * Request Lenses
    ucfnName,
    ucfnDescription,
    ucfnInstanceId,
    ucfnContactFlowId,

    -- * Destructuring the Response
    updateContactFlowNameResponse,
    UpdateContactFlowNameResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateContactFlowName' smart constructor.
data UpdateContactFlowName = UpdateContactFlowName'
  { _ucfnName ::
      !(Maybe Text),
    _ucfnDescription :: !(Maybe Text),
    _ucfnInstanceId :: !Text,
    _ucfnContactFlowId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateContactFlowName' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucfnName' - The name of the contact flow.
--
-- * 'ucfnDescription' - The description of the contact flow.
--
-- * 'ucfnInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'ucfnContactFlowId' - The identifier of the contact flow.
updateContactFlowName ::
  -- | 'ucfnInstanceId'
  Text ->
  -- | 'ucfnContactFlowId'
  Text ->
  UpdateContactFlowName
updateContactFlowName pInstanceId_ pContactFlowId_ =
  UpdateContactFlowName'
    { _ucfnName = Nothing,
      _ucfnDescription = Nothing,
      _ucfnInstanceId = pInstanceId_,
      _ucfnContactFlowId = pContactFlowId_
    }

-- | The name of the contact flow.
ucfnName :: Lens' UpdateContactFlowName (Maybe Text)
ucfnName = lens _ucfnName (\s a -> s {_ucfnName = a})

-- | The description of the contact flow.
ucfnDescription :: Lens' UpdateContactFlowName (Maybe Text)
ucfnDescription = lens _ucfnDescription (\s a -> s {_ucfnDescription = a})

-- | The identifier of the Amazon Connect instance.
ucfnInstanceId :: Lens' UpdateContactFlowName Text
ucfnInstanceId = lens _ucfnInstanceId (\s a -> s {_ucfnInstanceId = a})

-- | The identifier of the contact flow.
ucfnContactFlowId :: Lens' UpdateContactFlowName Text
ucfnContactFlowId = lens _ucfnContactFlowId (\s a -> s {_ucfnContactFlowId = a})

instance AWSRequest UpdateContactFlowName where
  type Rs UpdateContactFlowName = UpdateContactFlowNameResponse
  request = postJSON connect
  response = receiveNull UpdateContactFlowNameResponse'

instance Hashable UpdateContactFlowName

instance NFData UpdateContactFlowName

instance ToHeaders UpdateContactFlowName where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateContactFlowName where
  toJSON UpdateContactFlowName' {..} =
    object
      ( catMaybes
          [ ("Name" .=) <$> _ucfnName,
            ("Description" .=) <$> _ucfnDescription
          ]
      )

instance ToPath UpdateContactFlowName where
  toPath UpdateContactFlowName' {..} =
    mconcat
      [ "/contact-flows/",
        toBS _ucfnInstanceId,
        "/",
        toBS _ucfnContactFlowId,
        "/name"
      ]

instance ToQuery UpdateContactFlowName where
  toQuery = const mempty

-- | /See:/ 'updateContactFlowNameResponse' smart constructor.
data UpdateContactFlowNameResponse = UpdateContactFlowNameResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateContactFlowNameResponse' with the minimum fields required to make a request.
updateContactFlowNameResponse ::
  UpdateContactFlowNameResponse
updateContactFlowNameResponse = UpdateContactFlowNameResponse'

instance NFData UpdateContactFlowNameResponse
