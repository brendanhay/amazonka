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
-- Module      : Network.AWS.EC2.Types.IntegrateServices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IntegrateServices where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AthenaIntegration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes service integrations with VPC Flow logs.
--
-- /See:/ 'newIntegrateServices' smart constructor.
data IntegrateServices = IntegrateServices'
  { -- | Information about the integration with Amazon Athena.
    athenaIntegrations :: Prelude.Maybe (Prelude.NonEmpty AthenaIntegration)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntegrateServices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'athenaIntegrations', 'integrateServices_athenaIntegrations' - Information about the integration with Amazon Athena.
newIntegrateServices ::
  IntegrateServices
newIntegrateServices =
  IntegrateServices'
    { athenaIntegrations =
        Prelude.Nothing
    }

-- | Information about the integration with Amazon Athena.
integrateServices_athenaIntegrations :: Lens.Lens' IntegrateServices (Prelude.Maybe (Prelude.NonEmpty AthenaIntegration))
integrateServices_athenaIntegrations = Lens.lens (\IntegrateServices' {athenaIntegrations} -> athenaIntegrations) (\s@IntegrateServices' {} a -> s {athenaIntegrations = a} :: IntegrateServices) Prelude.. Lens.mapping Lens._Coerce

instance Prelude.Hashable IntegrateServices

instance Prelude.NFData IntegrateServices

instance Core.ToQuery IntegrateServices where
  toQuery IntegrateServices' {..} =
    Prelude.mconcat
      [ Core.toQuery
          ( Core.toQueryList "AthenaIntegration"
              Prelude.<$> athenaIntegrations
          )
      ]
