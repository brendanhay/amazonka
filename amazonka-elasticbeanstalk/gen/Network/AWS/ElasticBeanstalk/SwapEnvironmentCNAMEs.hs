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

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    sourceEnvironmentName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the destination environment.
    --
    -- Condition: You must specify at least the @DestinationEnvironmentID@ or
    -- the @DestinationEnvironmentName@. You may also specify both. You must
    -- specify the @SourceEnvironmentId@ with the @DestinationEnvironmentId@.
    destinationEnvironmentId :: Prelude.Maybe Prelude.Text,
    -- | The name of the destination environment.
    --
    -- Condition: You must specify at least the @DestinationEnvironmentID@ or
    -- the @DestinationEnvironmentName@. You may also specify both. You must
    -- specify the @SourceEnvironmentName@ with the
    -- @DestinationEnvironmentName@.
    destinationEnvironmentName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the source environment.
    --
    -- Condition: You must specify at least the @SourceEnvironmentID@ or the
    -- @SourceEnvironmentName@. You may also specify both. If you specify the
    -- @SourceEnvironmentId@, you must specify the @DestinationEnvironmentId@.
    sourceEnvironmentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      destinationEnvironmentId = Prelude.Nothing,
      destinationEnvironmentName = Prelude.Nothing,
      sourceEnvironmentId = Prelude.Nothing
    }

-- | The name of the source environment.
--
-- Condition: You must specify at least the @SourceEnvironmentID@ or the
-- @SourceEnvironmentName@. You may also specify both. If you specify the
-- @SourceEnvironmentName@, you must specify the
-- @DestinationEnvironmentName@.
swapEnvironmentCNAMEs_sourceEnvironmentName :: Lens.Lens' SwapEnvironmentCNAMEs (Prelude.Maybe Prelude.Text)
swapEnvironmentCNAMEs_sourceEnvironmentName = Lens.lens (\SwapEnvironmentCNAMEs' {sourceEnvironmentName} -> sourceEnvironmentName) (\s@SwapEnvironmentCNAMEs' {} a -> s {sourceEnvironmentName = a} :: SwapEnvironmentCNAMEs)

-- | The ID of the destination environment.
--
-- Condition: You must specify at least the @DestinationEnvironmentID@ or
-- the @DestinationEnvironmentName@. You may also specify both. You must
-- specify the @SourceEnvironmentId@ with the @DestinationEnvironmentId@.
swapEnvironmentCNAMEs_destinationEnvironmentId :: Lens.Lens' SwapEnvironmentCNAMEs (Prelude.Maybe Prelude.Text)
swapEnvironmentCNAMEs_destinationEnvironmentId = Lens.lens (\SwapEnvironmentCNAMEs' {destinationEnvironmentId} -> destinationEnvironmentId) (\s@SwapEnvironmentCNAMEs' {} a -> s {destinationEnvironmentId = a} :: SwapEnvironmentCNAMEs)

-- | The name of the destination environment.
--
-- Condition: You must specify at least the @DestinationEnvironmentID@ or
-- the @DestinationEnvironmentName@. You may also specify both. You must
-- specify the @SourceEnvironmentName@ with the
-- @DestinationEnvironmentName@.
swapEnvironmentCNAMEs_destinationEnvironmentName :: Lens.Lens' SwapEnvironmentCNAMEs (Prelude.Maybe Prelude.Text)
swapEnvironmentCNAMEs_destinationEnvironmentName = Lens.lens (\SwapEnvironmentCNAMEs' {destinationEnvironmentName} -> destinationEnvironmentName) (\s@SwapEnvironmentCNAMEs' {} a -> s {destinationEnvironmentName = a} :: SwapEnvironmentCNAMEs)

-- | The ID of the source environment.
--
-- Condition: You must specify at least the @SourceEnvironmentID@ or the
-- @SourceEnvironmentName@. You may also specify both. If you specify the
-- @SourceEnvironmentId@, you must specify the @DestinationEnvironmentId@.
swapEnvironmentCNAMEs_sourceEnvironmentId :: Lens.Lens' SwapEnvironmentCNAMEs (Prelude.Maybe Prelude.Text)
swapEnvironmentCNAMEs_sourceEnvironmentId = Lens.lens (\SwapEnvironmentCNAMEs' {sourceEnvironmentId} -> sourceEnvironmentId) (\s@SwapEnvironmentCNAMEs' {} a -> s {sourceEnvironmentId = a} :: SwapEnvironmentCNAMEs)

instance Prelude.AWSRequest SwapEnvironmentCNAMEs where
  type
    Rs SwapEnvironmentCNAMEs =
      SwapEnvironmentCNAMEsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull SwapEnvironmentCNAMEsResponse'

instance Prelude.Hashable SwapEnvironmentCNAMEs

instance Prelude.NFData SwapEnvironmentCNAMEs

instance Prelude.ToHeaders SwapEnvironmentCNAMEs where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath SwapEnvironmentCNAMEs where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SwapEnvironmentCNAMEs where
  toQuery SwapEnvironmentCNAMEs' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("SwapEnvironmentCNAMEs" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "SourceEnvironmentName"
          Prelude.=: sourceEnvironmentName,
        "DestinationEnvironmentId"
          Prelude.=: destinationEnvironmentId,
        "DestinationEnvironmentName"
          Prelude.=: destinationEnvironmentName,
        "SourceEnvironmentId" Prelude.=: sourceEnvironmentId
      ]

-- | /See:/ 'newSwapEnvironmentCNAMEsResponse' smart constructor.
data SwapEnvironmentCNAMEsResponse = SwapEnvironmentCNAMEsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SwapEnvironmentCNAMEsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSwapEnvironmentCNAMEsResponse ::
  SwapEnvironmentCNAMEsResponse
newSwapEnvironmentCNAMEsResponse =
  SwapEnvironmentCNAMEsResponse'

instance Prelude.NFData SwapEnvironmentCNAMEsResponse
