{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeDeploy.Types.EC2TagSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.EC2TagSet where

import Network.AWS.CodeDeploy.Types.EC2TagFilter
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about groups of EC2 instance tags.
--
-- /See:/ 'newEC2TagSet' smart constructor.
data EC2TagSet = EC2TagSet'
  { -- | A list that contains other lists of EC2 instance tag groups. For an
    -- instance to be included in the deployment group, it must be identified
    -- by all of the tag groups in the list.
    ec2TagSetList :: Prelude.Maybe [[EC2TagFilter]]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EC2TagSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ec2TagSetList', 'eC2TagSet_ec2TagSetList' - A list that contains other lists of EC2 instance tag groups. For an
-- instance to be included in the deployment group, it must be identified
-- by all of the tag groups in the list.
newEC2TagSet ::
  EC2TagSet
newEC2TagSet =
  EC2TagSet' {ec2TagSetList = Prelude.Nothing}

-- | A list that contains other lists of EC2 instance tag groups. For an
-- instance to be included in the deployment group, it must be identified
-- by all of the tag groups in the list.
eC2TagSet_ec2TagSetList :: Lens.Lens' EC2TagSet (Prelude.Maybe [[EC2TagFilter]])
eC2TagSet_ec2TagSetList = Lens.lens (\EC2TagSet' {ec2TagSetList} -> ec2TagSetList) (\s@EC2TagSet' {} a -> s {ec2TagSetList = a} :: EC2TagSet) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON EC2TagSet where
  parseJSON =
    Prelude.withObject
      "EC2TagSet"
      ( \x ->
          EC2TagSet'
            Prelude.<$> ( x Prelude..:? "ec2TagSetList"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable EC2TagSet

instance Prelude.NFData EC2TagSet

instance Prelude.ToJSON EC2TagSet where
  toJSON EC2TagSet' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ec2TagSetList" Prelude..=)
              Prelude.<$> ec2TagSetList
          ]
      )
