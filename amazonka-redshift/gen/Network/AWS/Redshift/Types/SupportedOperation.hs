{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

-- | Describes the operations that are allowed on a maintenance track.
--
-- /See:/ 'newSupportedOperation' smart constructor.
data SupportedOperation = SupportedOperation'
  { -- | A list of the supported operations.
    operationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  SupportedOperation'
    { operationName =
        Prelude.Nothing
    }

-- | A list of the supported operations.
supportedOperation_operationName :: Lens.Lens' SupportedOperation (Prelude.Maybe Prelude.Text)
supportedOperation_operationName = Lens.lens (\SupportedOperation' {operationName} -> operationName) (\s@SupportedOperation' {} a -> s {operationName = a} :: SupportedOperation)

instance Prelude.FromXML SupportedOperation where
  parseXML x =
    SupportedOperation'
      Prelude.<$> (x Prelude..@? "OperationName")

instance Prelude.Hashable SupportedOperation

instance Prelude.NFData SupportedOperation
