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
-- Module      : Amazonka.EC2.Types.EfaInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.EfaInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the Elastic Fabric Adapters for the instance type.
--
-- /See:/ 'newEfaInfo' smart constructor.
data EfaInfo = EfaInfo'
  { -- | The maximum number of Elastic Fabric Adapters for the instance type.
    maximumEfaInterfaces :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EfaInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumEfaInterfaces', 'efaInfo_maximumEfaInterfaces' - The maximum number of Elastic Fabric Adapters for the instance type.
newEfaInfo ::
  EfaInfo
newEfaInfo =
  EfaInfo' {maximumEfaInterfaces = Prelude.Nothing}

-- | The maximum number of Elastic Fabric Adapters for the instance type.
efaInfo_maximumEfaInterfaces :: Lens.Lens' EfaInfo (Prelude.Maybe Prelude.Int)
efaInfo_maximumEfaInterfaces = Lens.lens (\EfaInfo' {maximumEfaInterfaces} -> maximumEfaInterfaces) (\s@EfaInfo' {} a -> s {maximumEfaInterfaces = a} :: EfaInfo)

instance Data.FromXML EfaInfo where
  parseXML x =
    EfaInfo'
      Prelude.<$> (x Data..@? "maximumEfaInterfaces")

instance Prelude.Hashable EfaInfo where
  hashWithSalt _salt EfaInfo' {..} =
    _salt `Prelude.hashWithSalt` maximumEfaInterfaces

instance Prelude.NFData EfaInfo where
  rnf EfaInfo' {..} = Prelude.rnf maximumEfaInterfaces
