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
-- Module      : Network.AWS.Connect.AssociateApprovedOrigin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an approved origin to an Amazon Connect instance.
module Network.AWS.Connect.AssociateApprovedOrigin
  ( -- * Creating a Request
    associateApprovedOrigin,
    AssociateApprovedOrigin,

    -- * Request Lenses
    aaoInstanceId,
    aaoOrigin,

    -- * Destructuring the Response
    associateApprovedOriginResponse,
    AssociateApprovedOriginResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateApprovedOrigin' smart constructor.
data AssociateApprovedOrigin = AssociateApprovedOrigin'
  { _aaoInstanceId ::
      !Text,
    _aaoOrigin :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateApprovedOrigin' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaoInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'aaoOrigin' - The domain to add to your allow list.
associateApprovedOrigin ::
  -- | 'aaoInstanceId'
  Text ->
  -- | 'aaoOrigin'
  Text ->
  AssociateApprovedOrigin
associateApprovedOrigin pInstanceId_ pOrigin_ =
  AssociateApprovedOrigin'
    { _aaoInstanceId = pInstanceId_,
      _aaoOrigin = pOrigin_
    }

-- | The identifier of the Amazon Connect instance.
aaoInstanceId :: Lens' AssociateApprovedOrigin Text
aaoInstanceId = lens _aaoInstanceId (\s a -> s {_aaoInstanceId = a})

-- | The domain to add to your allow list.
aaoOrigin :: Lens' AssociateApprovedOrigin Text
aaoOrigin = lens _aaoOrigin (\s a -> s {_aaoOrigin = a})

instance AWSRequest AssociateApprovedOrigin where
  type Rs AssociateApprovedOrigin = AssociateApprovedOriginResponse
  request = putJSON connect
  response = receiveNull AssociateApprovedOriginResponse'

instance Hashable AssociateApprovedOrigin

instance NFData AssociateApprovedOrigin

instance ToHeaders AssociateApprovedOrigin where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON AssociateApprovedOrigin where
  toJSON AssociateApprovedOrigin' {..} =
    object (catMaybes [Just ("Origin" .= _aaoOrigin)])

instance ToPath AssociateApprovedOrigin where
  toPath AssociateApprovedOrigin' {..} =
    mconcat ["/instance/", toBS _aaoInstanceId, "/approved-origin"]

instance ToQuery AssociateApprovedOrigin where
  toQuery = const mempty

-- | /See:/ 'associateApprovedOriginResponse' smart constructor.
data AssociateApprovedOriginResponse = AssociateApprovedOriginResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateApprovedOriginResponse' with the minimum fields required to make a request.
associateApprovedOriginResponse ::
  AssociateApprovedOriginResponse
associateApprovedOriginResponse = AssociateApprovedOriginResponse'

instance NFData AssociateApprovedOriginResponse
