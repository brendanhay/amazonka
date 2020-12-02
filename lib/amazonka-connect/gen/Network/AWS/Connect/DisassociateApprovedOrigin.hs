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
-- Module      : Network.AWS.Connect.DisassociateApprovedOrigin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes access to integrated applications from Amazon Connect.
module Network.AWS.Connect.DisassociateApprovedOrigin
  ( -- * Creating a Request
    disassociateApprovedOrigin,
    DisassociateApprovedOrigin,

    -- * Request Lenses
    daoInstanceId,
    daoOrigin,

    -- * Destructuring the Response
    disassociateApprovedOriginResponse,
    DisassociateApprovedOriginResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateApprovedOrigin' smart constructor.
data DisassociateApprovedOrigin = DisassociateApprovedOrigin'
  { _daoInstanceId ::
      !Text,
    _daoOrigin :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateApprovedOrigin' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daoInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'daoOrigin' - The domain URL of the integrated application.
disassociateApprovedOrigin ::
  -- | 'daoInstanceId'
  Text ->
  -- | 'daoOrigin'
  Text ->
  DisassociateApprovedOrigin
disassociateApprovedOrigin pInstanceId_ pOrigin_ =
  DisassociateApprovedOrigin'
    { _daoInstanceId = pInstanceId_,
      _daoOrigin = pOrigin_
    }

-- | The identifier of the Amazon Connect instance.
daoInstanceId :: Lens' DisassociateApprovedOrigin Text
daoInstanceId = lens _daoInstanceId (\s a -> s {_daoInstanceId = a})

-- | The domain URL of the integrated application.
daoOrigin :: Lens' DisassociateApprovedOrigin Text
daoOrigin = lens _daoOrigin (\s a -> s {_daoOrigin = a})

instance AWSRequest DisassociateApprovedOrigin where
  type
    Rs DisassociateApprovedOrigin =
      DisassociateApprovedOriginResponse
  request = delete connect
  response = receiveNull DisassociateApprovedOriginResponse'

instance Hashable DisassociateApprovedOrigin

instance NFData DisassociateApprovedOrigin

instance ToHeaders DisassociateApprovedOrigin where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DisassociateApprovedOrigin where
  toPath DisassociateApprovedOrigin' {..} =
    mconcat ["/instance/", toBS _daoInstanceId, "/approved-origin"]

instance ToQuery DisassociateApprovedOrigin where
  toQuery DisassociateApprovedOrigin' {..} =
    mconcat ["origin" =: _daoOrigin]

-- | /See:/ 'disassociateApprovedOriginResponse' smart constructor.
data DisassociateApprovedOriginResponse = DisassociateApprovedOriginResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateApprovedOriginResponse' with the minimum fields required to make a request.
disassociateApprovedOriginResponse ::
  DisassociateApprovedOriginResponse
disassociateApprovedOriginResponse =
  DisassociateApprovedOriginResponse'

instance NFData DisassociateApprovedOriginResponse
