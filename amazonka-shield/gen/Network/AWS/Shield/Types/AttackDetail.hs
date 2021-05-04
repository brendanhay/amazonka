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
-- Module      : Network.AWS.Shield.Types.AttackDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.AttackDetail where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Shield.Types.AttackProperty
import Network.AWS.Shield.Types.Mitigation
import Network.AWS.Shield.Types.SubResourceSummary
import Network.AWS.Shield.Types.SummarizedCounter

-- | The details of a DDoS attack.
--
-- /See:/ 'newAttackDetail' smart constructor.
data AttackDetail = AttackDetail'
  { -- | The ARN (Amazon Resource Name) of the resource that was attacked.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The array of AttackProperty objects.
    attackProperties :: Prelude.Maybe [AttackProperty],
    -- | The time the attack started, in Unix time in seconds. For more
    -- information see
    -- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The time the attack ended, in Unix time in seconds. For more information
    -- see
    -- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
    endTime :: Prelude.Maybe Prelude.POSIX,
    -- | List of counters that describe the attack for the specified time period.
    attackCounters :: Prelude.Maybe [SummarizedCounter],
    -- | List of mitigation actions taken for the attack.
    mitigations :: Prelude.Maybe [Mitigation],
    -- | The unique identifier (ID) of the attack.
    attackId :: Prelude.Maybe Prelude.Text,
    -- | If applicable, additional detail about the resource being attacked, for
    -- example, IP address or URL.
    subResources :: Prelude.Maybe [SubResourceSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AttackDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'attackDetail_resourceArn' - The ARN (Amazon Resource Name) of the resource that was attacked.
--
-- 'attackProperties', 'attackDetail_attackProperties' - The array of AttackProperty objects.
--
-- 'startTime', 'attackDetail_startTime' - The time the attack started, in Unix time in seconds. For more
-- information see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
--
-- 'endTime', 'attackDetail_endTime' - The time the attack ended, in Unix time in seconds. For more information
-- see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
--
-- 'attackCounters', 'attackDetail_attackCounters' - List of counters that describe the attack for the specified time period.
--
-- 'mitigations', 'attackDetail_mitigations' - List of mitigation actions taken for the attack.
--
-- 'attackId', 'attackDetail_attackId' - The unique identifier (ID) of the attack.
--
-- 'subResources', 'attackDetail_subResources' - If applicable, additional detail about the resource being attacked, for
-- example, IP address or URL.
newAttackDetail ::
  AttackDetail
newAttackDetail =
  AttackDetail'
    { resourceArn = Prelude.Nothing,
      attackProperties = Prelude.Nothing,
      startTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      attackCounters = Prelude.Nothing,
      mitigations = Prelude.Nothing,
      attackId = Prelude.Nothing,
      subResources = Prelude.Nothing
    }

-- | The ARN (Amazon Resource Name) of the resource that was attacked.
attackDetail_resourceArn :: Lens.Lens' AttackDetail (Prelude.Maybe Prelude.Text)
attackDetail_resourceArn = Lens.lens (\AttackDetail' {resourceArn} -> resourceArn) (\s@AttackDetail' {} a -> s {resourceArn = a} :: AttackDetail)

-- | The array of AttackProperty objects.
attackDetail_attackProperties :: Lens.Lens' AttackDetail (Prelude.Maybe [AttackProperty])
attackDetail_attackProperties = Lens.lens (\AttackDetail' {attackProperties} -> attackProperties) (\s@AttackDetail' {} a -> s {attackProperties = a} :: AttackDetail) Prelude.. Lens.mapping Prelude._Coerce

-- | The time the attack started, in Unix time in seconds. For more
-- information see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
attackDetail_startTime :: Lens.Lens' AttackDetail (Prelude.Maybe Prelude.UTCTime)
attackDetail_startTime = Lens.lens (\AttackDetail' {startTime} -> startTime) (\s@AttackDetail' {} a -> s {startTime = a} :: AttackDetail) Prelude.. Lens.mapping Prelude._Time

-- | The time the attack ended, in Unix time in seconds. For more information
-- see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
attackDetail_endTime :: Lens.Lens' AttackDetail (Prelude.Maybe Prelude.UTCTime)
attackDetail_endTime = Lens.lens (\AttackDetail' {endTime} -> endTime) (\s@AttackDetail' {} a -> s {endTime = a} :: AttackDetail) Prelude.. Lens.mapping Prelude._Time

-- | List of counters that describe the attack for the specified time period.
attackDetail_attackCounters :: Lens.Lens' AttackDetail (Prelude.Maybe [SummarizedCounter])
attackDetail_attackCounters = Lens.lens (\AttackDetail' {attackCounters} -> attackCounters) (\s@AttackDetail' {} a -> s {attackCounters = a} :: AttackDetail) Prelude.. Lens.mapping Prelude._Coerce

-- | List of mitigation actions taken for the attack.
attackDetail_mitigations :: Lens.Lens' AttackDetail (Prelude.Maybe [Mitigation])
attackDetail_mitigations = Lens.lens (\AttackDetail' {mitigations} -> mitigations) (\s@AttackDetail' {} a -> s {mitigations = a} :: AttackDetail) Prelude.. Lens.mapping Prelude._Coerce

-- | The unique identifier (ID) of the attack.
attackDetail_attackId :: Lens.Lens' AttackDetail (Prelude.Maybe Prelude.Text)
attackDetail_attackId = Lens.lens (\AttackDetail' {attackId} -> attackId) (\s@AttackDetail' {} a -> s {attackId = a} :: AttackDetail)

-- | If applicable, additional detail about the resource being attacked, for
-- example, IP address or URL.
attackDetail_subResources :: Lens.Lens' AttackDetail (Prelude.Maybe [SubResourceSummary])
attackDetail_subResources = Lens.lens (\AttackDetail' {subResources} -> subResources) (\s@AttackDetail' {} a -> s {subResources = a} :: AttackDetail) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON AttackDetail where
  parseJSON =
    Prelude.withObject
      "AttackDetail"
      ( \x ->
          AttackDetail'
            Prelude.<$> (x Prelude..:? "ResourceArn")
            Prelude.<*> ( x Prelude..:? "AttackProperties"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "StartTime")
            Prelude.<*> (x Prelude..:? "EndTime")
            Prelude.<*> ( x Prelude..:? "AttackCounters"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "Mitigations"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "AttackId")
            Prelude.<*> ( x Prelude..:? "SubResources"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AttackDetail

instance Prelude.NFData AttackDetail
