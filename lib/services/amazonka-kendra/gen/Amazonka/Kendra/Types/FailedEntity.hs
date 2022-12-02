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
-- Module      : Amazonka.Kendra.Types.FailedEntity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.FailedEntity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information on the users or groups in your IAM Identity Center identity
-- source that failed to properly configure with your Amazon Kendra
-- experience.
--
-- /See:/ 'newFailedEntity' smart constructor.
data FailedEntity = FailedEntity'
  { -- | The identifier of the user or group in your IAM Identity Center identity
    -- source. For example, a user ID could be an email.
    entityId :: Prelude.Maybe Prelude.Text,
    -- | The reason the user or group in your IAM Identity Center identity source
    -- failed to properly configure with your Amazon Kendra experience.
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityId', 'failedEntity_entityId' - The identifier of the user or group in your IAM Identity Center identity
-- source. For example, a user ID could be an email.
--
-- 'errorMessage', 'failedEntity_errorMessage' - The reason the user or group in your IAM Identity Center identity source
-- failed to properly configure with your Amazon Kendra experience.
newFailedEntity ::
  FailedEntity
newFailedEntity =
  FailedEntity'
    { entityId = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The identifier of the user or group in your IAM Identity Center identity
-- source. For example, a user ID could be an email.
failedEntity_entityId :: Lens.Lens' FailedEntity (Prelude.Maybe Prelude.Text)
failedEntity_entityId = Lens.lens (\FailedEntity' {entityId} -> entityId) (\s@FailedEntity' {} a -> s {entityId = a} :: FailedEntity)

-- | The reason the user or group in your IAM Identity Center identity source
-- failed to properly configure with your Amazon Kendra experience.
failedEntity_errorMessage :: Lens.Lens' FailedEntity (Prelude.Maybe Prelude.Text)
failedEntity_errorMessage = Lens.lens (\FailedEntity' {errorMessage} -> errorMessage) (\s@FailedEntity' {} a -> s {errorMessage = a} :: FailedEntity)

instance Data.FromJSON FailedEntity where
  parseJSON =
    Data.withObject
      "FailedEntity"
      ( \x ->
          FailedEntity'
            Prelude.<$> (x Data..:? "EntityId")
            Prelude.<*> (x Data..:? "ErrorMessage")
      )

instance Prelude.Hashable FailedEntity where
  hashWithSalt _salt FailedEntity' {..} =
    _salt `Prelude.hashWithSalt` entityId
      `Prelude.hashWithSalt` errorMessage

instance Prelude.NFData FailedEntity where
  rnf FailedEntity' {..} =
    Prelude.rnf entityId
      `Prelude.seq` Prelude.rnf errorMessage
