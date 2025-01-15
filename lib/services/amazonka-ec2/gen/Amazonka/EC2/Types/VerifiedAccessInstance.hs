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
-- Module      : Amazonka.EC2.Types.VerifiedAccessInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VerifiedAccessInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.VerifiedAccessTrustProviderCondensed
import qualified Amazonka.Prelude as Prelude

-- | Describes a Verified Access instance.
--
-- /See:/ 'newVerifiedAccessInstance' smart constructor.
data VerifiedAccessInstance = VerifiedAccessInstance'
  { -- | The creation time.
    creationTime :: Prelude.Maybe Prelude.Text,
    -- | A description for the Amazon Web Services Verified Access instance.
    description :: Prelude.Maybe Prelude.Text,
    -- | The last updated time.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text,
    -- | The tags.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the Amazon Web Services Verified Access instance.
    verifiedAccessInstanceId :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the Amazon Web Services Verified Access trust providers.
    verifiedAccessTrustProviders :: Prelude.Maybe [VerifiedAccessTrustProviderCondensed]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifiedAccessInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'verifiedAccessInstance_creationTime' - The creation time.
--
-- 'description', 'verifiedAccessInstance_description' - A description for the Amazon Web Services Verified Access instance.
--
-- 'lastUpdatedTime', 'verifiedAccessInstance_lastUpdatedTime' - The last updated time.
--
-- 'tags', 'verifiedAccessInstance_tags' - The tags.
--
-- 'verifiedAccessInstanceId', 'verifiedAccessInstance_verifiedAccessInstanceId' - The ID of the Amazon Web Services Verified Access instance.
--
-- 'verifiedAccessTrustProviders', 'verifiedAccessInstance_verifiedAccessTrustProviders' - The IDs of the Amazon Web Services Verified Access trust providers.
newVerifiedAccessInstance ::
  VerifiedAccessInstance
newVerifiedAccessInstance =
  VerifiedAccessInstance'
    { creationTime =
        Prelude.Nothing,
      description = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      tags = Prelude.Nothing,
      verifiedAccessInstanceId = Prelude.Nothing,
      verifiedAccessTrustProviders = Prelude.Nothing
    }

-- | The creation time.
verifiedAccessInstance_creationTime :: Lens.Lens' VerifiedAccessInstance (Prelude.Maybe Prelude.Text)
verifiedAccessInstance_creationTime = Lens.lens (\VerifiedAccessInstance' {creationTime} -> creationTime) (\s@VerifiedAccessInstance' {} a -> s {creationTime = a} :: VerifiedAccessInstance)

-- | A description for the Amazon Web Services Verified Access instance.
verifiedAccessInstance_description :: Lens.Lens' VerifiedAccessInstance (Prelude.Maybe Prelude.Text)
verifiedAccessInstance_description = Lens.lens (\VerifiedAccessInstance' {description} -> description) (\s@VerifiedAccessInstance' {} a -> s {description = a} :: VerifiedAccessInstance)

-- | The last updated time.
verifiedAccessInstance_lastUpdatedTime :: Lens.Lens' VerifiedAccessInstance (Prelude.Maybe Prelude.Text)
verifiedAccessInstance_lastUpdatedTime = Lens.lens (\VerifiedAccessInstance' {lastUpdatedTime} -> lastUpdatedTime) (\s@VerifiedAccessInstance' {} a -> s {lastUpdatedTime = a} :: VerifiedAccessInstance)

-- | The tags.
verifiedAccessInstance_tags :: Lens.Lens' VerifiedAccessInstance (Prelude.Maybe [Tag])
verifiedAccessInstance_tags = Lens.lens (\VerifiedAccessInstance' {tags} -> tags) (\s@VerifiedAccessInstance' {} a -> s {tags = a} :: VerifiedAccessInstance) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services Verified Access instance.
verifiedAccessInstance_verifiedAccessInstanceId :: Lens.Lens' VerifiedAccessInstance (Prelude.Maybe Prelude.Text)
verifiedAccessInstance_verifiedAccessInstanceId = Lens.lens (\VerifiedAccessInstance' {verifiedAccessInstanceId} -> verifiedAccessInstanceId) (\s@VerifiedAccessInstance' {} a -> s {verifiedAccessInstanceId = a} :: VerifiedAccessInstance)

-- | The IDs of the Amazon Web Services Verified Access trust providers.
verifiedAccessInstance_verifiedAccessTrustProviders :: Lens.Lens' VerifiedAccessInstance (Prelude.Maybe [VerifiedAccessTrustProviderCondensed])
verifiedAccessInstance_verifiedAccessTrustProviders = Lens.lens (\VerifiedAccessInstance' {verifiedAccessTrustProviders} -> verifiedAccessTrustProviders) (\s@VerifiedAccessInstance' {} a -> s {verifiedAccessTrustProviders = a} :: VerifiedAccessInstance) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML VerifiedAccessInstance where
  parseXML x =
    VerifiedAccessInstance'
      Prelude.<$> (x Data..@? "creationTime")
      Prelude.<*> (x Data..@? "description")
      Prelude.<*> (x Data..@? "lastUpdatedTime")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "verifiedAccessInstanceId")
      Prelude.<*> ( x
                      Data..@? "verifiedAccessTrustProviderSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable VerifiedAccessInstance where
  hashWithSalt _salt VerifiedAccessInstance' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` verifiedAccessInstanceId
      `Prelude.hashWithSalt` verifiedAccessTrustProviders

instance Prelude.NFData VerifiedAccessInstance where
  rnf VerifiedAccessInstance' {..} =
    Prelude.rnf creationTime `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf lastUpdatedTime `Prelude.seq`
          Prelude.rnf tags `Prelude.seq`
            Prelude.rnf verifiedAccessInstanceId `Prelude.seq`
              Prelude.rnf verifiedAccessTrustProviders
