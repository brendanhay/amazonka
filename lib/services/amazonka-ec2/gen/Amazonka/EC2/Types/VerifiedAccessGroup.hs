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
-- Module      : Amazonka.EC2.Types.VerifiedAccessGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VerifiedAccessGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a Verified Access group.
--
-- /See:/ 'newVerifiedAccessGroup' smart constructor.
data VerifiedAccessGroup = VerifiedAccessGroup'
  { -- | The creation time.
    creationTime :: Prelude.Maybe Prelude.Text,
    -- | The deletion time.
    deletionTime :: Prelude.Maybe Prelude.Text,
    -- | A description for the Amazon Web Services Verified Access group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The last updated time.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account number that owns the group.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The tags.
    tags :: Prelude.Maybe [Tag],
    -- | The ARN of the Verified Access group.
    verifiedAccessGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Verified Access group.
    verifiedAccessGroupId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services Verified Access instance.
    verifiedAccessInstanceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifiedAccessGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'verifiedAccessGroup_creationTime' - The creation time.
--
-- 'deletionTime', 'verifiedAccessGroup_deletionTime' - The deletion time.
--
-- 'description', 'verifiedAccessGroup_description' - A description for the Amazon Web Services Verified Access group.
--
-- 'lastUpdatedTime', 'verifiedAccessGroup_lastUpdatedTime' - The last updated time.
--
-- 'owner', 'verifiedAccessGroup_owner' - The Amazon Web Services account number that owns the group.
--
-- 'tags', 'verifiedAccessGroup_tags' - The tags.
--
-- 'verifiedAccessGroupArn', 'verifiedAccessGroup_verifiedAccessGroupArn' - The ARN of the Verified Access group.
--
-- 'verifiedAccessGroupId', 'verifiedAccessGroup_verifiedAccessGroupId' - The ID of the Verified Access group.
--
-- 'verifiedAccessInstanceId', 'verifiedAccessGroup_verifiedAccessInstanceId' - The ID of the Amazon Web Services Verified Access instance.
newVerifiedAccessGroup ::
  VerifiedAccessGroup
newVerifiedAccessGroup =
  VerifiedAccessGroup'
    { creationTime =
        Prelude.Nothing,
      deletionTime = Prelude.Nothing,
      description = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      owner = Prelude.Nothing,
      tags = Prelude.Nothing,
      verifiedAccessGroupArn = Prelude.Nothing,
      verifiedAccessGroupId = Prelude.Nothing,
      verifiedAccessInstanceId = Prelude.Nothing
    }

-- | The creation time.
verifiedAccessGroup_creationTime :: Lens.Lens' VerifiedAccessGroup (Prelude.Maybe Prelude.Text)
verifiedAccessGroup_creationTime = Lens.lens (\VerifiedAccessGroup' {creationTime} -> creationTime) (\s@VerifiedAccessGroup' {} a -> s {creationTime = a} :: VerifiedAccessGroup)

-- | The deletion time.
verifiedAccessGroup_deletionTime :: Lens.Lens' VerifiedAccessGroup (Prelude.Maybe Prelude.Text)
verifiedAccessGroup_deletionTime = Lens.lens (\VerifiedAccessGroup' {deletionTime} -> deletionTime) (\s@VerifiedAccessGroup' {} a -> s {deletionTime = a} :: VerifiedAccessGroup)

-- | A description for the Amazon Web Services Verified Access group.
verifiedAccessGroup_description :: Lens.Lens' VerifiedAccessGroup (Prelude.Maybe Prelude.Text)
verifiedAccessGroup_description = Lens.lens (\VerifiedAccessGroup' {description} -> description) (\s@VerifiedAccessGroup' {} a -> s {description = a} :: VerifiedAccessGroup)

-- | The last updated time.
verifiedAccessGroup_lastUpdatedTime :: Lens.Lens' VerifiedAccessGroup (Prelude.Maybe Prelude.Text)
verifiedAccessGroup_lastUpdatedTime = Lens.lens (\VerifiedAccessGroup' {lastUpdatedTime} -> lastUpdatedTime) (\s@VerifiedAccessGroup' {} a -> s {lastUpdatedTime = a} :: VerifiedAccessGroup)

-- | The Amazon Web Services account number that owns the group.
verifiedAccessGroup_owner :: Lens.Lens' VerifiedAccessGroup (Prelude.Maybe Prelude.Text)
verifiedAccessGroup_owner = Lens.lens (\VerifiedAccessGroup' {owner} -> owner) (\s@VerifiedAccessGroup' {} a -> s {owner = a} :: VerifiedAccessGroup)

-- | The tags.
verifiedAccessGroup_tags :: Lens.Lens' VerifiedAccessGroup (Prelude.Maybe [Tag])
verifiedAccessGroup_tags = Lens.lens (\VerifiedAccessGroup' {tags} -> tags) (\s@VerifiedAccessGroup' {} a -> s {tags = a} :: VerifiedAccessGroup) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the Verified Access group.
verifiedAccessGroup_verifiedAccessGroupArn :: Lens.Lens' VerifiedAccessGroup (Prelude.Maybe Prelude.Text)
verifiedAccessGroup_verifiedAccessGroupArn = Lens.lens (\VerifiedAccessGroup' {verifiedAccessGroupArn} -> verifiedAccessGroupArn) (\s@VerifiedAccessGroup' {} a -> s {verifiedAccessGroupArn = a} :: VerifiedAccessGroup)

-- | The ID of the Verified Access group.
verifiedAccessGroup_verifiedAccessGroupId :: Lens.Lens' VerifiedAccessGroup (Prelude.Maybe Prelude.Text)
verifiedAccessGroup_verifiedAccessGroupId = Lens.lens (\VerifiedAccessGroup' {verifiedAccessGroupId} -> verifiedAccessGroupId) (\s@VerifiedAccessGroup' {} a -> s {verifiedAccessGroupId = a} :: VerifiedAccessGroup)

-- | The ID of the Amazon Web Services Verified Access instance.
verifiedAccessGroup_verifiedAccessInstanceId :: Lens.Lens' VerifiedAccessGroup (Prelude.Maybe Prelude.Text)
verifiedAccessGroup_verifiedAccessInstanceId = Lens.lens (\VerifiedAccessGroup' {verifiedAccessInstanceId} -> verifiedAccessInstanceId) (\s@VerifiedAccessGroup' {} a -> s {verifiedAccessInstanceId = a} :: VerifiedAccessGroup)

instance Data.FromXML VerifiedAccessGroup where
  parseXML x =
    VerifiedAccessGroup'
      Prelude.<$> (x Data..@? "creationTime")
      Prelude.<*> (x Data..@? "deletionTime")
      Prelude.<*> (x Data..@? "description")
      Prelude.<*> (x Data..@? "lastUpdatedTime")
      Prelude.<*> (x Data..@? "owner")
      Prelude.<*> ( x
                      Data..@? "tagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "verifiedAccessGroupArn")
      Prelude.<*> (x Data..@? "verifiedAccessGroupId")
      Prelude.<*> (x Data..@? "verifiedAccessInstanceId")

instance Prelude.Hashable VerifiedAccessGroup where
  hashWithSalt _salt VerifiedAccessGroup' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` deletionTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` verifiedAccessGroupArn
      `Prelude.hashWithSalt` verifiedAccessGroupId
      `Prelude.hashWithSalt` verifiedAccessInstanceId

instance Prelude.NFData VerifiedAccessGroup where
  rnf VerifiedAccessGroup' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf deletionTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf verifiedAccessGroupArn
      `Prelude.seq` Prelude.rnf verifiedAccessGroupId
      `Prelude.seq` Prelude.rnf verifiedAccessInstanceId
