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
-- Module      : Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Swaps the CNAMEs of two environments.
module Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs
  ( -- * Creating a Request
    SwapEnvironmentCNAMEs (..),
    newSwapEnvironmentCNAMEs,

    -- * Request Lenses
    swapEnvironmentCNAMEs_sourceEnvironmentName,
    swapEnvironmentCNAMEs_destinationEnvironmentId,
    swapEnvironmentCNAMEs_destinationEnvironmentName,
    swapEnvironmentCNAMEs_sourceEnvironmentId,

    -- * Destructuring the Response
    SwapEnvironmentCNAMEsResponse (..),
    newSwapEnvironmentCNAMEsResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Swaps the CNAMEs of two environments.
--
-- /See:/ 'newSwapEnvironmentCNAMEs' smart constructor.
data SwapEnvironmentCNAMEs = SwapEnvironmentCNAMEs'
  { -- | The name of the source environment.
    --
    -- Condition: You must specify at least the @SourceEnvironmentID@ or the
    -- @SourceEnvironmentName@. You may also specify both. If you specify the
    -- @SourceEnvironmentName@, you must specify the
    -- @DestinationEnvironmentName@.
    sourceEnvironmentName :: Core.Maybe Core.Text,
    -- | The ID of the destination environment.
    --
    -- Condition: You must specify at least the @DestinationEnvironmentID@ or
    -- the @DestinationEnvironmentName@. You may also specify both. You must
    -- specify the @SourceEnvironmentId@ with the @DestinationEnvironmentId@.
    destinationEnvironmentId :: Core.Maybe Core.Text,
    -- | The name of the destination environment.
    --
    -- Condition: You must specify at least the @DestinationEnvironmentID@ or
    -- the @DestinationEnvironmentName@. You may also specify both. You must
    -- specify the @SourceEnvironmentName@ with the
    -- @DestinationEnvironmentName@.
    destinationEnvironmentName :: Core.Maybe Core.Text,
    -- | The ID of the source environment.
    --
    -- Condition: You must specify at least the @SourceEnvironmentID@ or the
    -- @SourceEnvironmentName@. You may also specify both. If you specify the
    -- @SourceEnvironmentId@, you must specify the @DestinationEnvironmentId@.
    sourceEnvironmentId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SwapEnvironmentCNAMEs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceEnvironmentName', 'swapEnvironmentCNAMEs_sourceEnvironmentName' - The name of the source environment.
--
-- Condition: You must specify at least the @SourceEnvironmentID@ or the
-- @SourceEnvironmentName@. You may also specify both. If you specify the
-- @SourceEnvironmentName@, you must specify the
-- @DestinationEnvironmentName@.
--
-- 'destinationEnvironmentId', 'swapEnvironmentCNAMEs_destinationEnvironmentId' - The ID of the destination environment.
--
-- Condition: You must specify at least the @DestinationEnvironmentID@ or
-- the @DestinationEnvironmentName@. You may also specify both. You must
-- specify the @SourceEnvironmentId@ with the @DestinationEnvironmentId@.
--
-- 'destinationEnvironmentName', 'swapEnvironmentCNAMEs_destinationEnvironmentName' - The name of the destination environment.
--
-- Condition: You must specify at least the @DestinationEnvironmentID@ or
-- the @DestinationEnvironmentName@. You may also specify both. You must
-- specify the @SourceEnvironmentName@ with the
-- @DestinationEnvironmentName@.
--
-- 'sourceEnvironmentId', 'swapEnvironmentCNAMEs_sourceEnvironmentId' - The ID of the source environment.
--
-- Condition: You must specify at least the @SourceEnvironmentID@ or the
-- @SourceEnvironmentName@. You may also specify both. If you specify the
-- @SourceEnvironmentId@, you must specify the @DestinationEnvironmentId@.
newSwapEnvironmentCNAMEs ::
  SwapEnvironmentCNAMEs
newSwapEnvironmentCNAMEs =
  SwapEnvironmentCNAMEs'
    { sourceEnvironmentName =
        Core.Nothing,
      destinationEnvironmentId = Core.Nothing,
      destinationEnvironmentName = Core.Nothing,
      sourceEnvironmentId = Core.Nothing
    }

-- | The name of the source environment.
--
-- Condition: You must specify at least the @SourceEnvironmentID@ or the
-- @SourceEnvironmentName@. You may also specify both. If you specify the
-- @SourceEnvironmentName@, you must specify the
-- @DestinationEnvironmentName@.
swapEnvironmentCNAMEs_sourceEnvironmentName :: Lens.Lens' SwapEnvironmentCNAMEs (Core.Maybe Core.Text)
swapEnvironmentCNAMEs_sourceEnvironmentName = Lens.lens (\SwapEnvironmentCNAMEs' {sourceEnvironmentName} -> sourceEnvironmentName) (\s@SwapEnvironmentCNAMEs' {} a -> s {sourceEnvironmentName = a} :: SwapEnvironmentCNAMEs)

-- | The ID of the destination environment.
--
-- Condition: You must specify at least the @DestinationEnvironmentID@ or
-- the @DestinationEnvironmentName@. You may also specify both. You must
-- specify the @SourceEnvironmentId@ with the @DestinationEnvironmentId@.
swapEnvironmentCNAMEs_destinationEnvironmentId :: Lens.Lens' SwapEnvironmentCNAMEs (Core.Maybe Core.Text)
swapEnvironmentCNAMEs_destinationEnvironmentId = Lens.lens (\SwapEnvironmentCNAMEs' {destinationEnvironmentId} -> destinationEnvironmentId) (\s@SwapEnvironmentCNAMEs' {} a -> s {destinationEnvironmentId = a} :: SwapEnvironmentCNAMEs)

-- | The name of the destination environment.
--
-- Condition: You must specify at least the @DestinationEnvironmentID@ or
-- the @DestinationEnvironmentName@. You may also specify both. You must
-- specify the @SourceEnvironmentName@ with the
-- @DestinationEnvironmentName@.
swapEnvironmentCNAMEs_destinationEnvironmentName :: Lens.Lens' SwapEnvironmentCNAMEs (Core.Maybe Core.Text)
swapEnvironmentCNAMEs_destinationEnvironmentName = Lens.lens (\SwapEnvironmentCNAMEs' {destinationEnvironmentName} -> destinationEnvironmentName) (\s@SwapEnvironmentCNAMEs' {} a -> s {destinationEnvironmentName = a} :: SwapEnvironmentCNAMEs)

-- | The ID of the source environment.
--
-- Condition: You must specify at least the @SourceEnvironmentID@ or the
-- @SourceEnvironmentName@. You may also specify both. If you specify the
-- @SourceEnvironmentId@, you must specify the @DestinationEnvironmentId@.
swapEnvironmentCNAMEs_sourceEnvironmentId :: Lens.Lens' SwapEnvironmentCNAMEs (Core.Maybe Core.Text)
swapEnvironmentCNAMEs_sourceEnvironmentId = Lens.lens (\SwapEnvironmentCNAMEs' {sourceEnvironmentId} -> sourceEnvironmentId) (\s@SwapEnvironmentCNAMEs' {} a -> s {sourceEnvironmentId = a} :: SwapEnvironmentCNAMEs)

instance Core.AWSRequest SwapEnvironmentCNAMEs where
  type
    AWSResponse SwapEnvironmentCNAMEs =
      SwapEnvironmentCNAMEsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull SwapEnvironmentCNAMEsResponse'

instance Core.Hashable SwapEnvironmentCNAMEs

instance Core.NFData SwapEnvironmentCNAMEs

instance Core.ToHeaders SwapEnvironmentCNAMEs where
  toHeaders = Core.const Core.mempty

instance Core.ToPath SwapEnvironmentCNAMEs where
  toPath = Core.const "/"

instance Core.ToQuery SwapEnvironmentCNAMEs where
  toQuery SwapEnvironmentCNAMEs' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("SwapEnvironmentCNAMEs" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "SourceEnvironmentName"
          Core.=: sourceEnvironmentName,
        "DestinationEnvironmentId"
          Core.=: destinationEnvironmentId,
        "DestinationEnvironmentName"
          Core.=: destinationEnvironmentName,
        "SourceEnvironmentId" Core.=: sourceEnvironmentId
      ]

-- | /See:/ 'newSwapEnvironmentCNAMEsResponse' smart constructor.
data SwapEnvironmentCNAMEsResponse = SwapEnvironmentCNAMEsResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SwapEnvironmentCNAMEsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSwapEnvironmentCNAMEsResponse ::
  SwapEnvironmentCNAMEsResponse
newSwapEnvironmentCNAMEsResponse =
  SwapEnvironmentCNAMEsResponse'

instance Core.NFData SwapEnvironmentCNAMEsResponse
