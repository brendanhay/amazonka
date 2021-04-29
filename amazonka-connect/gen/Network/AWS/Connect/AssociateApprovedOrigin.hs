{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.AssociateApprovedOrigin
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Associates an approved origin to an Amazon Connect instance.
module Network.AWS.Connect.AssociateApprovedOrigin
  ( -- * Creating a Request
    AssociateApprovedOrigin (..),
    newAssociateApprovedOrigin,

    -- * Request Lenses
    associateApprovedOrigin_instanceId,
    associateApprovedOrigin_origin,

    -- * Destructuring the Response
    AssociateApprovedOriginResponse (..),
    newAssociateApprovedOriginResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateApprovedOrigin' smart constructor.
data AssociateApprovedOrigin = AssociateApprovedOrigin'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The domain to add to your allow list.
    origin :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateApprovedOrigin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'associateApprovedOrigin_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'origin', 'associateApprovedOrigin_origin' - The domain to add to your allow list.
newAssociateApprovedOrigin ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'origin'
  Prelude.Text ->
  AssociateApprovedOrigin
newAssociateApprovedOrigin pInstanceId_ pOrigin_ =
  AssociateApprovedOrigin'
    { instanceId = pInstanceId_,
      origin = pOrigin_
    }

-- | The identifier of the Amazon Connect instance.
associateApprovedOrigin_instanceId :: Lens.Lens' AssociateApprovedOrigin Prelude.Text
associateApprovedOrigin_instanceId = Lens.lens (\AssociateApprovedOrigin' {instanceId} -> instanceId) (\s@AssociateApprovedOrigin' {} a -> s {instanceId = a} :: AssociateApprovedOrigin)

-- | The domain to add to your allow list.
associateApprovedOrigin_origin :: Lens.Lens' AssociateApprovedOrigin Prelude.Text
associateApprovedOrigin_origin = Lens.lens (\AssociateApprovedOrigin' {origin} -> origin) (\s@AssociateApprovedOrigin' {} a -> s {origin = a} :: AssociateApprovedOrigin)

instance Prelude.AWSRequest AssociateApprovedOrigin where
  type
    Rs AssociateApprovedOrigin =
      AssociateApprovedOriginResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveNull
      AssociateApprovedOriginResponse'

instance Prelude.Hashable AssociateApprovedOrigin

instance Prelude.NFData AssociateApprovedOrigin

instance Prelude.ToHeaders AssociateApprovedOrigin where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AssociateApprovedOrigin where
  toJSON AssociateApprovedOrigin' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Origin" Prelude..= origin)]
      )

instance Prelude.ToPath AssociateApprovedOrigin where
  toPath AssociateApprovedOrigin' {..} =
    Prelude.mconcat
      [ "/instance/",
        Prelude.toBS instanceId,
        "/approved-origin"
      ]

instance Prelude.ToQuery AssociateApprovedOrigin where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateApprovedOriginResponse' smart constructor.
data AssociateApprovedOriginResponse = AssociateApprovedOriginResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateApprovedOriginResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateApprovedOriginResponse ::
  AssociateApprovedOriginResponse
newAssociateApprovedOriginResponse =
  AssociateApprovedOriginResponse'

instance
  Prelude.NFData
    AssociateApprovedOriginResponse
