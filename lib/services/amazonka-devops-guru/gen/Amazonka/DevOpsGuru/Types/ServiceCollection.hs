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
-- Module      : Amazonka.DevOpsGuru.Types.ServiceCollection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ServiceCollection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.ServiceName
import qualified Amazonka.Prelude as Prelude

-- | A collection of the names of Amazon Web Services services.
--
-- /See:/ 'newServiceCollection' smart constructor.
data ServiceCollection = ServiceCollection'
  { -- | An array of strings that each specifies the name of an Amazon Web
    -- Services service.
    serviceNames :: Prelude.Maybe [ServiceName]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceNames', 'serviceCollection_serviceNames' - An array of strings that each specifies the name of an Amazon Web
-- Services service.
newServiceCollection ::
  ServiceCollection
newServiceCollection =
  ServiceCollection' {serviceNames = Prelude.Nothing}

-- | An array of strings that each specifies the name of an Amazon Web
-- Services service.
serviceCollection_serviceNames :: Lens.Lens' ServiceCollection (Prelude.Maybe [ServiceName])
serviceCollection_serviceNames = Lens.lens (\ServiceCollection' {serviceNames} -> serviceNames) (\s@ServiceCollection' {} a -> s {serviceNames = a} :: ServiceCollection) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ServiceCollection where
  parseJSON =
    Data.withObject
      "ServiceCollection"
      ( \x ->
          ServiceCollection'
            Prelude.<$> (x Data..:? "ServiceNames" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ServiceCollection where
  hashWithSalt _salt ServiceCollection' {..} =
    _salt `Prelude.hashWithSalt` serviceNames

instance Prelude.NFData ServiceCollection where
  rnf ServiceCollection' {..} = Prelude.rnf serviceNames

instance Data.ToJSON ServiceCollection where
  toJSON ServiceCollection' {..} =
    Data.object
      ( Prelude.catMaybes
          [("ServiceNames" Data..=) Prelude.<$> serviceNames]
      )
