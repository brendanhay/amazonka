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
-- Module      : Amazonka.MediaTailor.Types.Alert
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.Alert where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Alert configuration parameters.
--
-- /See:/ 'newAlert' smart constructor.
data Alert = Alert'
  { -- | The Amazon Resource Name (ARN) of the resource.
    resourceArn :: Prelude.Text,
    -- | The code for the alert. For example, NOT_PROCESSED.
    alertCode :: Prelude.Text,
    -- | The timestamp when the alert was last modified.
    lastModifiedTime :: Core.POSIX,
    -- | The Amazon Resource Names (ARNs) related to this alert.
    relatedResourceArns :: [Prelude.Text],
    -- | If an alert is generated for a resource, an explanation of the reason
    -- for the alert.
    alertMessage :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Alert' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'alert_resourceArn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'alertCode', 'alert_alertCode' - The code for the alert. For example, NOT_PROCESSED.
--
-- 'lastModifiedTime', 'alert_lastModifiedTime' - The timestamp when the alert was last modified.
--
-- 'relatedResourceArns', 'alert_relatedResourceArns' - The Amazon Resource Names (ARNs) related to this alert.
--
-- 'alertMessage', 'alert_alertMessage' - If an alert is generated for a resource, an explanation of the reason
-- for the alert.
newAlert ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'alertCode'
  Prelude.Text ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'alertMessage'
  Prelude.Text ->
  Alert
newAlert
  pResourceArn_
  pAlertCode_
  pLastModifiedTime_
  pAlertMessage_ =
    Alert'
      { resourceArn = pResourceArn_,
        alertCode = pAlertCode_,
        lastModifiedTime =
          Core._Time Lens.# pLastModifiedTime_,
        relatedResourceArns = Prelude.mempty,
        alertMessage = pAlertMessage_
      }

-- | The Amazon Resource Name (ARN) of the resource.
alert_resourceArn :: Lens.Lens' Alert Prelude.Text
alert_resourceArn = Lens.lens (\Alert' {resourceArn} -> resourceArn) (\s@Alert' {} a -> s {resourceArn = a} :: Alert)

-- | The code for the alert. For example, NOT_PROCESSED.
alert_alertCode :: Lens.Lens' Alert Prelude.Text
alert_alertCode = Lens.lens (\Alert' {alertCode} -> alertCode) (\s@Alert' {} a -> s {alertCode = a} :: Alert)

-- | The timestamp when the alert was last modified.
alert_lastModifiedTime :: Lens.Lens' Alert Prelude.UTCTime
alert_lastModifiedTime = Lens.lens (\Alert' {lastModifiedTime} -> lastModifiedTime) (\s@Alert' {} a -> s {lastModifiedTime = a} :: Alert) Prelude.. Core._Time

-- | The Amazon Resource Names (ARNs) related to this alert.
alert_relatedResourceArns :: Lens.Lens' Alert [Prelude.Text]
alert_relatedResourceArns = Lens.lens (\Alert' {relatedResourceArns} -> relatedResourceArns) (\s@Alert' {} a -> s {relatedResourceArns = a} :: Alert) Prelude.. Lens.coerced

-- | If an alert is generated for a resource, an explanation of the reason
-- for the alert.
alert_alertMessage :: Lens.Lens' Alert Prelude.Text
alert_alertMessage = Lens.lens (\Alert' {alertMessage} -> alertMessage) (\s@Alert' {} a -> s {alertMessage = a} :: Alert)

instance Core.FromJSON Alert where
  parseJSON =
    Core.withObject
      "Alert"
      ( \x ->
          Alert'
            Prelude.<$> (x Core..: "ResourceArn")
            Prelude.<*> (x Core..: "AlertCode")
            Prelude.<*> (x Core..: "LastModifiedTime")
            Prelude.<*> ( x Core..:? "RelatedResourceArns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "AlertMessage")
      )

instance Prelude.Hashable Alert where
  hashWithSalt _salt Alert' {..} =
    _salt `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` alertCode
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` relatedResourceArns
      `Prelude.hashWithSalt` alertMessage

instance Prelude.NFData Alert where
  rnf Alert' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf alertCode
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf relatedResourceArns
      `Prelude.seq` Prelude.rnf alertMessage
