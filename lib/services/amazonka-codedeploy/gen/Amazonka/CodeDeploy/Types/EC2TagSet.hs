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
-- Module      : Amazonka.CodeDeploy.Types.EC2TagSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.EC2TagSet where

import Amazonka.CodeDeploy.Types.EC2TagFilter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about groups of Amazon EC2 instance tags.
--
-- /See:/ 'newEC2TagSet' smart constructor.
data EC2TagSet = EC2TagSet'
  { -- | A list that contains other lists of Amazon EC2 instance tag groups. For
    -- an instance to be included in the deployment group, it must be
    -- identified by all of the tag groups in the list.
    ec2TagSetList :: Prelude.Maybe [[EC2TagFilter]]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EC2TagSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ec2TagSetList', 'eC2TagSet_ec2TagSetList' - A list that contains other lists of Amazon EC2 instance tag groups. For
-- an instance to be included in the deployment group, it must be
-- identified by all of the tag groups in the list.
newEC2TagSet ::
  EC2TagSet
newEC2TagSet =
  EC2TagSet' {ec2TagSetList = Prelude.Nothing}

-- | A list that contains other lists of Amazon EC2 instance tag groups. For
-- an instance to be included in the deployment group, it must be
-- identified by all of the tag groups in the list.
eC2TagSet_ec2TagSetList :: Lens.Lens' EC2TagSet (Prelude.Maybe [[EC2TagFilter]])
eC2TagSet_ec2TagSetList = Lens.lens (\EC2TagSet' {ec2TagSetList} -> ec2TagSetList) (\s@EC2TagSet' {} a -> s {ec2TagSetList = a} :: EC2TagSet) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON EC2TagSet where
  parseJSON =
    Data.withObject
      "EC2TagSet"
      ( \x ->
          EC2TagSet'
            Prelude.<$> (x Data..:? "ec2TagSetList" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable EC2TagSet where
  hashWithSalt _salt EC2TagSet' {..} =
    _salt `Prelude.hashWithSalt` ec2TagSetList

instance Prelude.NFData EC2TagSet where
  rnf EC2TagSet' {..} = Prelude.rnf ec2TagSetList

instance Data.ToJSON EC2TagSet where
  toJSON EC2TagSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ec2TagSetList" Data..=)
              Prelude.<$> ec2TagSetList
          ]
      )
