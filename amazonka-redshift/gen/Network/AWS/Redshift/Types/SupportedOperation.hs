{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.SupportedOperation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.SupportedOperation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal

-- | Describes the operations that are allowed on a maintenance track.
--
-- /See:/ 'newSupportedOperation' smart constructor.
data SupportedOperation = SupportedOperation'
  { -- | A list of the supported operations.
    operationName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SupportedOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationName', 'supportedOperation_operationName' - A list of the supported operations.
newSupportedOperation ::
  SupportedOperation
newSupportedOperation =
  SupportedOperation' {operationName = Core.Nothing}

-- | A list of the supported operations.
supportedOperation_operationName :: Lens.Lens' SupportedOperation (Core.Maybe Core.Text)
supportedOperation_operationName = Lens.lens (\SupportedOperation' {operationName} -> operationName) (\s@SupportedOperation' {} a -> s {operationName = a} :: SupportedOperation)

instance Core.FromXML SupportedOperation where
  parseXML x =
    SupportedOperation'
      Core.<$> (x Core..@? "OperationName")

instance Core.Hashable SupportedOperation

instance Core.NFData SupportedOperation
