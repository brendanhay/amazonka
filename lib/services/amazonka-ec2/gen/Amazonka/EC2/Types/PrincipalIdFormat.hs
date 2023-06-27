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
-- Module      : Amazonka.EC2.Types.PrincipalIdFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PrincipalIdFormat where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.IdFormat
import qualified Amazonka.Prelude as Prelude

-- | PrincipalIdFormat description
--
-- /See:/ 'newPrincipalIdFormat' smart constructor.
data PrincipalIdFormat = PrincipalIdFormat'
  { -- | PrincipalIdFormatARN description
    arn :: Prelude.Maybe Prelude.Text,
    -- | PrincipalIdFormatStatuses description
    statuses :: Prelude.Maybe [IdFormat]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrincipalIdFormat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'principalIdFormat_arn' - PrincipalIdFormatARN description
--
-- 'statuses', 'principalIdFormat_statuses' - PrincipalIdFormatStatuses description
newPrincipalIdFormat ::
  PrincipalIdFormat
newPrincipalIdFormat =
  PrincipalIdFormat'
    { arn = Prelude.Nothing,
      statuses = Prelude.Nothing
    }

-- | PrincipalIdFormatARN description
principalIdFormat_arn :: Lens.Lens' PrincipalIdFormat (Prelude.Maybe Prelude.Text)
principalIdFormat_arn = Lens.lens (\PrincipalIdFormat' {arn} -> arn) (\s@PrincipalIdFormat' {} a -> s {arn = a} :: PrincipalIdFormat)

-- | PrincipalIdFormatStatuses description
principalIdFormat_statuses :: Lens.Lens' PrincipalIdFormat (Prelude.Maybe [IdFormat])
principalIdFormat_statuses = Lens.lens (\PrincipalIdFormat' {statuses} -> statuses) (\s@PrincipalIdFormat' {} a -> s {statuses = a} :: PrincipalIdFormat) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML PrincipalIdFormat where
  parseXML x =
    PrincipalIdFormat'
      Prelude.<$> (x Data..@? "arn")
      Prelude.<*> ( x
                      Data..@? "statusSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable PrincipalIdFormat where
  hashWithSalt _salt PrincipalIdFormat' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` statuses

instance Prelude.NFData PrincipalIdFormat where
  rnf PrincipalIdFormat' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf statuses
