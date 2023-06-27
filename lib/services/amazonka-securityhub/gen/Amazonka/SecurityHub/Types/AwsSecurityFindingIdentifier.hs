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
-- Module      : Amazonka.SecurityHub.Types.AwsSecurityFindingIdentifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsSecurityFindingIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Identifies which finding to get the finding history for.
--
-- /See:/ 'newAwsSecurityFindingIdentifier' smart constructor.
data AwsSecurityFindingIdentifier = AwsSecurityFindingIdentifier'
  { -- | The identifier of the finding that was specified by the finding
    -- provider.
    id :: Prelude.Text,
    -- | The ARN generated by Security Hub that uniquely identifies a product
    -- that generates findings. This can be the ARN for a third-party product
    -- that is integrated with Security Hub, or the ARN for a custom
    -- integration.
    productArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsSecurityFindingIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'awsSecurityFindingIdentifier_id' - The identifier of the finding that was specified by the finding
-- provider.
--
-- 'productArn', 'awsSecurityFindingIdentifier_productArn' - The ARN generated by Security Hub that uniquely identifies a product
-- that generates findings. This can be the ARN for a third-party product
-- that is integrated with Security Hub, or the ARN for a custom
-- integration.
newAwsSecurityFindingIdentifier ::
  -- | 'id'
  Prelude.Text ->
  -- | 'productArn'
  Prelude.Text ->
  AwsSecurityFindingIdentifier
newAwsSecurityFindingIdentifier pId_ pProductArn_ =
  AwsSecurityFindingIdentifier'
    { id = pId_,
      productArn = pProductArn_
    }

-- | The identifier of the finding that was specified by the finding
-- provider.
awsSecurityFindingIdentifier_id :: Lens.Lens' AwsSecurityFindingIdentifier Prelude.Text
awsSecurityFindingIdentifier_id = Lens.lens (\AwsSecurityFindingIdentifier' {id} -> id) (\s@AwsSecurityFindingIdentifier' {} a -> s {id = a} :: AwsSecurityFindingIdentifier)

-- | The ARN generated by Security Hub that uniquely identifies a product
-- that generates findings. This can be the ARN for a third-party product
-- that is integrated with Security Hub, or the ARN for a custom
-- integration.
awsSecurityFindingIdentifier_productArn :: Lens.Lens' AwsSecurityFindingIdentifier Prelude.Text
awsSecurityFindingIdentifier_productArn = Lens.lens (\AwsSecurityFindingIdentifier' {productArn} -> productArn) (\s@AwsSecurityFindingIdentifier' {} a -> s {productArn = a} :: AwsSecurityFindingIdentifier)

instance Data.FromJSON AwsSecurityFindingIdentifier where
  parseJSON =
    Data.withObject
      "AwsSecurityFindingIdentifier"
      ( \x ->
          AwsSecurityFindingIdentifier'
            Prelude.<$> (x Data..: "Id")
            Prelude.<*> (x Data..: "ProductArn")
      )

instance
  Prelude.Hashable
    AwsSecurityFindingIdentifier
  where
  hashWithSalt _salt AwsSecurityFindingIdentifier' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` productArn

instance Prelude.NFData AwsSecurityFindingIdentifier where
  rnf AwsSecurityFindingIdentifier' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf productArn

instance Data.ToJSON AwsSecurityFindingIdentifier where
  toJSON AwsSecurityFindingIdentifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Data..= id),
            Prelude.Just ("ProductArn" Data..= productArn)
          ]
      )
