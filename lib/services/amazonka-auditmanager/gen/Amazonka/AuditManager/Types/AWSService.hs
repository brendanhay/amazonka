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
-- Module      : Amazonka.AuditManager.Types.AWSService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.AWSService where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An Amazon Web Service such as Amazon S3 or CloudTrail.
--
-- /See:/ 'newAWSService' smart constructor.
data AWSService = AWSService'
  { -- | The name of the Amazon Web Service.
    serviceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AWSService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceName', 'aWSService_serviceName' - The name of the Amazon Web Service.
newAWSService ::
  AWSService
newAWSService =
  AWSService' {serviceName = Prelude.Nothing}

-- | The name of the Amazon Web Service.
aWSService_serviceName :: Lens.Lens' AWSService (Prelude.Maybe Prelude.Text)
aWSService_serviceName = Lens.lens (\AWSService' {serviceName} -> serviceName) (\s@AWSService' {} a -> s {serviceName = a} :: AWSService)

instance Data.FromJSON AWSService where
  parseJSON =
    Data.withObject
      "AWSService"
      ( \x ->
          AWSService' Prelude.<$> (x Data..:? "serviceName")
      )

instance Prelude.Hashable AWSService where
  hashWithSalt _salt AWSService' {..} =
    _salt `Prelude.hashWithSalt` serviceName

instance Prelude.NFData AWSService where
  rnf AWSService' {..} = Prelude.rnf serviceName

instance Data.ToJSON AWSService where
  toJSON AWSService' {..} =
    Data.object
      ( Prelude.catMaybes
          [("serviceName" Data..=) Prelude.<$> serviceName]
      )
