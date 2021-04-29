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
-- Module      : Network.AWS.CodeDeploy.Types.InstanceInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.InstanceInfo where

import Network.AWS.CodeDeploy.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an on-premises instance.
--
-- /See:/ 'newInstanceInfo' smart constructor.
data InstanceInfo = InstanceInfo'
  { -- | The time at which the on-premises instance was registered.
    registerTime :: Prelude.Maybe Prelude.POSIX,
    -- | The IAM user ARN associated with the on-premises instance.
    iamUserArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the on-premises instance.
    instanceName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the on-premises instance.
    instanceArn :: Prelude.Maybe Prelude.Text,
    -- | The tags currently associated with the on-premises instance.
    tags :: Prelude.Maybe [Tag],
    -- | The ARN of the IAM session associated with the on-premises instance.
    iamSessionArn :: Prelude.Maybe Prelude.Text,
    -- | If the on-premises instance was deregistered, the time at which the
    -- on-premises instance was deregistered.
    deregisterTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstanceInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registerTime', 'instanceInfo_registerTime' - The time at which the on-premises instance was registered.
--
-- 'iamUserArn', 'instanceInfo_iamUserArn' - The IAM user ARN associated with the on-premises instance.
--
-- 'instanceName', 'instanceInfo_instanceName' - The name of the on-premises instance.
--
-- 'instanceArn', 'instanceInfo_instanceArn' - The ARN of the on-premises instance.
--
-- 'tags', 'instanceInfo_tags' - The tags currently associated with the on-premises instance.
--
-- 'iamSessionArn', 'instanceInfo_iamSessionArn' - The ARN of the IAM session associated with the on-premises instance.
--
-- 'deregisterTime', 'instanceInfo_deregisterTime' - If the on-premises instance was deregistered, the time at which the
-- on-premises instance was deregistered.
newInstanceInfo ::
  InstanceInfo
newInstanceInfo =
  InstanceInfo'
    { registerTime = Prelude.Nothing,
      iamUserArn = Prelude.Nothing,
      instanceName = Prelude.Nothing,
      instanceArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      iamSessionArn = Prelude.Nothing,
      deregisterTime = Prelude.Nothing
    }

-- | The time at which the on-premises instance was registered.
instanceInfo_registerTime :: Lens.Lens' InstanceInfo (Prelude.Maybe Prelude.UTCTime)
instanceInfo_registerTime = Lens.lens (\InstanceInfo' {registerTime} -> registerTime) (\s@InstanceInfo' {} a -> s {registerTime = a} :: InstanceInfo) Prelude.. Lens.mapping Prelude._Time

-- | The IAM user ARN associated with the on-premises instance.
instanceInfo_iamUserArn :: Lens.Lens' InstanceInfo (Prelude.Maybe Prelude.Text)
instanceInfo_iamUserArn = Lens.lens (\InstanceInfo' {iamUserArn} -> iamUserArn) (\s@InstanceInfo' {} a -> s {iamUserArn = a} :: InstanceInfo)

-- | The name of the on-premises instance.
instanceInfo_instanceName :: Lens.Lens' InstanceInfo (Prelude.Maybe Prelude.Text)
instanceInfo_instanceName = Lens.lens (\InstanceInfo' {instanceName} -> instanceName) (\s@InstanceInfo' {} a -> s {instanceName = a} :: InstanceInfo)

-- | The ARN of the on-premises instance.
instanceInfo_instanceArn :: Lens.Lens' InstanceInfo (Prelude.Maybe Prelude.Text)
instanceInfo_instanceArn = Lens.lens (\InstanceInfo' {instanceArn} -> instanceArn) (\s@InstanceInfo' {} a -> s {instanceArn = a} :: InstanceInfo)

-- | The tags currently associated with the on-premises instance.
instanceInfo_tags :: Lens.Lens' InstanceInfo (Prelude.Maybe [Tag])
instanceInfo_tags = Lens.lens (\InstanceInfo' {tags} -> tags) (\s@InstanceInfo' {} a -> s {tags = a} :: InstanceInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | The ARN of the IAM session associated with the on-premises instance.
instanceInfo_iamSessionArn :: Lens.Lens' InstanceInfo (Prelude.Maybe Prelude.Text)
instanceInfo_iamSessionArn = Lens.lens (\InstanceInfo' {iamSessionArn} -> iamSessionArn) (\s@InstanceInfo' {} a -> s {iamSessionArn = a} :: InstanceInfo)

-- | If the on-premises instance was deregistered, the time at which the
-- on-premises instance was deregistered.
instanceInfo_deregisterTime :: Lens.Lens' InstanceInfo (Prelude.Maybe Prelude.UTCTime)
instanceInfo_deregisterTime = Lens.lens (\InstanceInfo' {deregisterTime} -> deregisterTime) (\s@InstanceInfo' {} a -> s {deregisterTime = a} :: InstanceInfo) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON InstanceInfo where
  parseJSON =
    Prelude.withObject
      "InstanceInfo"
      ( \x ->
          InstanceInfo'
            Prelude.<$> (x Prelude..:? "registerTime")
            Prelude.<*> (x Prelude..:? "iamUserArn")
            Prelude.<*> (x Prelude..:? "instanceName")
            Prelude.<*> (x Prelude..:? "instanceArn")
            Prelude.<*> (x Prelude..:? "tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "iamSessionArn")
            Prelude.<*> (x Prelude..:? "deregisterTime")
      )

instance Prelude.Hashable InstanceInfo

instance Prelude.NFData InstanceInfo
