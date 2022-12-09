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
-- Module      : Amazonka.CodeDeploy.Types.InstanceInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.InstanceInfo where

import Amazonka.CodeDeploy.Types.Tag
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an on-premises instance.
--
-- /See:/ 'newInstanceInfo' smart constructor.
data InstanceInfo = InstanceInfo'
  { -- | If the on-premises instance was deregistered, the time at which the
    -- on-premises instance was deregistered.
    deregisterTime :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the IAM session associated with the on-premises instance.
    iamSessionArn :: Prelude.Maybe Prelude.Text,
    -- | The IAM user ARN associated with the on-premises instance.
    iamUserArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the on-premises instance.
    instanceArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the on-premises instance.
    instanceName :: Prelude.Maybe Prelude.Text,
    -- | The time at which the on-premises instance was registered.
    registerTime :: Prelude.Maybe Data.POSIX,
    -- | The tags currently associated with the on-premises instance.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deregisterTime', 'instanceInfo_deregisterTime' - If the on-premises instance was deregistered, the time at which the
-- on-premises instance was deregistered.
--
-- 'iamSessionArn', 'instanceInfo_iamSessionArn' - The ARN of the IAM session associated with the on-premises instance.
--
-- 'iamUserArn', 'instanceInfo_iamUserArn' - The IAM user ARN associated with the on-premises instance.
--
-- 'instanceArn', 'instanceInfo_instanceArn' - The ARN of the on-premises instance.
--
-- 'instanceName', 'instanceInfo_instanceName' - The name of the on-premises instance.
--
-- 'registerTime', 'instanceInfo_registerTime' - The time at which the on-premises instance was registered.
--
-- 'tags', 'instanceInfo_tags' - The tags currently associated with the on-premises instance.
newInstanceInfo ::
  InstanceInfo
newInstanceInfo =
  InstanceInfo'
    { deregisterTime = Prelude.Nothing,
      iamSessionArn = Prelude.Nothing,
      iamUserArn = Prelude.Nothing,
      instanceArn = Prelude.Nothing,
      instanceName = Prelude.Nothing,
      registerTime = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | If the on-premises instance was deregistered, the time at which the
-- on-premises instance was deregistered.
instanceInfo_deregisterTime :: Lens.Lens' InstanceInfo (Prelude.Maybe Prelude.UTCTime)
instanceInfo_deregisterTime = Lens.lens (\InstanceInfo' {deregisterTime} -> deregisterTime) (\s@InstanceInfo' {} a -> s {deregisterTime = a} :: InstanceInfo) Prelude.. Lens.mapping Data._Time

-- | The ARN of the IAM session associated with the on-premises instance.
instanceInfo_iamSessionArn :: Lens.Lens' InstanceInfo (Prelude.Maybe Prelude.Text)
instanceInfo_iamSessionArn = Lens.lens (\InstanceInfo' {iamSessionArn} -> iamSessionArn) (\s@InstanceInfo' {} a -> s {iamSessionArn = a} :: InstanceInfo)

-- | The IAM user ARN associated with the on-premises instance.
instanceInfo_iamUserArn :: Lens.Lens' InstanceInfo (Prelude.Maybe Prelude.Text)
instanceInfo_iamUserArn = Lens.lens (\InstanceInfo' {iamUserArn} -> iamUserArn) (\s@InstanceInfo' {} a -> s {iamUserArn = a} :: InstanceInfo)

-- | The ARN of the on-premises instance.
instanceInfo_instanceArn :: Lens.Lens' InstanceInfo (Prelude.Maybe Prelude.Text)
instanceInfo_instanceArn = Lens.lens (\InstanceInfo' {instanceArn} -> instanceArn) (\s@InstanceInfo' {} a -> s {instanceArn = a} :: InstanceInfo)

-- | The name of the on-premises instance.
instanceInfo_instanceName :: Lens.Lens' InstanceInfo (Prelude.Maybe Prelude.Text)
instanceInfo_instanceName = Lens.lens (\InstanceInfo' {instanceName} -> instanceName) (\s@InstanceInfo' {} a -> s {instanceName = a} :: InstanceInfo)

-- | The time at which the on-premises instance was registered.
instanceInfo_registerTime :: Lens.Lens' InstanceInfo (Prelude.Maybe Prelude.UTCTime)
instanceInfo_registerTime = Lens.lens (\InstanceInfo' {registerTime} -> registerTime) (\s@InstanceInfo' {} a -> s {registerTime = a} :: InstanceInfo) Prelude.. Lens.mapping Data._Time

-- | The tags currently associated with the on-premises instance.
instanceInfo_tags :: Lens.Lens' InstanceInfo (Prelude.Maybe [Tag])
instanceInfo_tags = Lens.lens (\InstanceInfo' {tags} -> tags) (\s@InstanceInfo' {} a -> s {tags = a} :: InstanceInfo) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON InstanceInfo where
  parseJSON =
    Data.withObject
      "InstanceInfo"
      ( \x ->
          InstanceInfo'
            Prelude.<$> (x Data..:? "deregisterTime")
            Prelude.<*> (x Data..:? "iamSessionArn")
            Prelude.<*> (x Data..:? "iamUserArn")
            Prelude.<*> (x Data..:? "instanceArn")
            Prelude.<*> (x Data..:? "instanceName")
            Prelude.<*> (x Data..:? "registerTime")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable InstanceInfo where
  hashWithSalt _salt InstanceInfo' {..} =
    _salt `Prelude.hashWithSalt` deregisterTime
      `Prelude.hashWithSalt` iamSessionArn
      `Prelude.hashWithSalt` iamUserArn
      `Prelude.hashWithSalt` instanceArn
      `Prelude.hashWithSalt` instanceName
      `Prelude.hashWithSalt` registerTime
      `Prelude.hashWithSalt` tags

instance Prelude.NFData InstanceInfo where
  rnf InstanceInfo' {..} =
    Prelude.rnf deregisterTime
      `Prelude.seq` Prelude.rnf iamSessionArn
      `Prelude.seq` Prelude.rnf iamUserArn
      `Prelude.seq` Prelude.rnf instanceArn
      `Prelude.seq` Prelude.rnf instanceName
      `Prelude.seq` Prelude.rnf registerTime
      `Prelude.seq` Prelude.rnf tags
