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
-- Module      : Amazonka.RAM.Types.Principal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.Principal where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a principal for use with Resource Access Manager.
--
-- /See:/ 'newPrincipal' smart constructor.
data Principal = Principal'
  { -- | The date and time when the principal was associated with the resource
    -- share.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Indicates whether the principal belongs to the same organization in
    -- Organizations as the Amazon Web Services account that owns the resource
    -- share.
    external :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the principal.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the association was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of a resource share the principal is associated with.
    resourceShareArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Principal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'principal_creationTime' - The date and time when the principal was associated with the resource
-- share.
--
-- 'external', 'principal_external' - Indicates whether the principal belongs to the same organization in
-- Organizations as the Amazon Web Services account that owns the resource
-- share.
--
-- 'id', 'principal_id' - The ID of the principal.
--
-- 'lastUpdatedTime', 'principal_lastUpdatedTime' - The date and time when the association was last updated.
--
-- 'resourceShareArn', 'principal_resourceShareArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of a resource share the principal is associated with.
newPrincipal ::
  Principal
newPrincipal =
  Principal'
    { creationTime = Prelude.Nothing,
      external = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      resourceShareArn = Prelude.Nothing
    }

-- | The date and time when the principal was associated with the resource
-- share.
principal_creationTime :: Lens.Lens' Principal (Prelude.Maybe Prelude.UTCTime)
principal_creationTime = Lens.lens (\Principal' {creationTime} -> creationTime) (\s@Principal' {} a -> s {creationTime = a} :: Principal) Prelude.. Lens.mapping Data._Time

-- | Indicates whether the principal belongs to the same organization in
-- Organizations as the Amazon Web Services account that owns the resource
-- share.
principal_external :: Lens.Lens' Principal (Prelude.Maybe Prelude.Bool)
principal_external = Lens.lens (\Principal' {external} -> external) (\s@Principal' {} a -> s {external = a} :: Principal)

-- | The ID of the principal.
principal_id :: Lens.Lens' Principal (Prelude.Maybe Prelude.Text)
principal_id = Lens.lens (\Principal' {id} -> id) (\s@Principal' {} a -> s {id = a} :: Principal)

-- | The date and time when the association was last updated.
principal_lastUpdatedTime :: Lens.Lens' Principal (Prelude.Maybe Prelude.UTCTime)
principal_lastUpdatedTime = Lens.lens (\Principal' {lastUpdatedTime} -> lastUpdatedTime) (\s@Principal' {} a -> s {lastUpdatedTime = a} :: Principal) Prelude.. Lens.mapping Data._Time

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of a resource share the principal is associated with.
principal_resourceShareArn :: Lens.Lens' Principal (Prelude.Maybe Prelude.Text)
principal_resourceShareArn = Lens.lens (\Principal' {resourceShareArn} -> resourceShareArn) (\s@Principal' {} a -> s {resourceShareArn = a} :: Principal)

instance Data.FromJSON Principal where
  parseJSON =
    Data.withObject
      "Principal"
      ( \x ->
          Principal'
            Prelude.<$> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "external")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "lastUpdatedTime")
            Prelude.<*> (x Data..:? "resourceShareArn")
      )

instance Prelude.Hashable Principal where
  hashWithSalt _salt Principal' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` external
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` resourceShareArn

instance Prelude.NFData Principal where
  rnf Principal' {..} =
    Prelude.rnf creationTime `Prelude.seq`
      Prelude.rnf external `Prelude.seq`
        Prelude.rnf id `Prelude.seq`
          Prelude.rnf lastUpdatedTime `Prelude.seq`
            Prelude.rnf resourceShareArn
