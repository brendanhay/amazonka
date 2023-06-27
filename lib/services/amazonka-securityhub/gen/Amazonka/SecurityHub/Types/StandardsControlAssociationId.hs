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
-- Module      : Amazonka.SecurityHub.Types.StandardsControlAssociationId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.StandardsControlAssociationId where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An array with one or more objects that includes a security control
-- (identified with @SecurityControlId@, @SecurityControlArn@, or a mix of
-- both parameters) and the Amazon Resource Name (ARN) of a standard. The
-- security control ID or ARN is the same across standards.
--
-- /See:/ 'newStandardsControlAssociationId' smart constructor.
data StandardsControlAssociationId = StandardsControlAssociationId'
  { -- | The unique identifier (identified with @SecurityControlId@,
    -- @SecurityControlArn@, or a mix of both parameters) of a security control
    -- across standards.
    securityControlId :: Prelude.Text,
    -- | The ARN of a standard.
    standardsArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StandardsControlAssociationId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityControlId', 'standardsControlAssociationId_securityControlId' - The unique identifier (identified with @SecurityControlId@,
-- @SecurityControlArn@, or a mix of both parameters) of a security control
-- across standards.
--
-- 'standardsArn', 'standardsControlAssociationId_standardsArn' - The ARN of a standard.
newStandardsControlAssociationId ::
  -- | 'securityControlId'
  Prelude.Text ->
  -- | 'standardsArn'
  Prelude.Text ->
  StandardsControlAssociationId
newStandardsControlAssociationId
  pSecurityControlId_
  pStandardsArn_ =
    StandardsControlAssociationId'
      { securityControlId =
          pSecurityControlId_,
        standardsArn = pStandardsArn_
      }

-- | The unique identifier (identified with @SecurityControlId@,
-- @SecurityControlArn@, or a mix of both parameters) of a security control
-- across standards.
standardsControlAssociationId_securityControlId :: Lens.Lens' StandardsControlAssociationId Prelude.Text
standardsControlAssociationId_securityControlId = Lens.lens (\StandardsControlAssociationId' {securityControlId} -> securityControlId) (\s@StandardsControlAssociationId' {} a -> s {securityControlId = a} :: StandardsControlAssociationId)

-- | The ARN of a standard.
standardsControlAssociationId_standardsArn :: Lens.Lens' StandardsControlAssociationId Prelude.Text
standardsControlAssociationId_standardsArn = Lens.lens (\StandardsControlAssociationId' {standardsArn} -> standardsArn) (\s@StandardsControlAssociationId' {} a -> s {standardsArn = a} :: StandardsControlAssociationId)

instance Data.FromJSON StandardsControlAssociationId where
  parseJSON =
    Data.withObject
      "StandardsControlAssociationId"
      ( \x ->
          StandardsControlAssociationId'
            Prelude.<$> (x Data..: "SecurityControlId")
            Prelude.<*> (x Data..: "StandardsArn")
      )

instance
  Prelude.Hashable
    StandardsControlAssociationId
  where
  hashWithSalt _salt StandardsControlAssociationId' {..} =
    _salt
      `Prelude.hashWithSalt` securityControlId
      `Prelude.hashWithSalt` standardsArn

instance Prelude.NFData StandardsControlAssociationId where
  rnf StandardsControlAssociationId' {..} =
    Prelude.rnf securityControlId
      `Prelude.seq` Prelude.rnf standardsArn

instance Data.ToJSON StandardsControlAssociationId where
  toJSON StandardsControlAssociationId' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SecurityControlId" Data..= securityControlId),
            Prelude.Just ("StandardsArn" Data..= standardsArn)
          ]
      )
