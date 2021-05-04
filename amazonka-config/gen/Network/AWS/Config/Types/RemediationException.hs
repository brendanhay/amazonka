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
-- Module      : Network.AWS.Config.Types.RemediationException
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationException where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that represents the details about the remediation exception.
-- The details include the rule name, an explanation of an exception, the
-- time when the exception will be deleted, the resource ID, and resource
-- type.
--
-- /See:/ 'newRemediationException' smart constructor.
data RemediationException = RemediationException'
  { -- | The time when the remediation exception will be deleted.
    expirationTime :: Prelude.Maybe Prelude.POSIX,
    -- | An explanation of an remediation exception.
    message :: Prelude.Maybe Prelude.Text,
    -- | The name of the AWS Config rule.
    configRuleName :: Prelude.Text,
    -- | The type of a resource.
    resourceType :: Prelude.Text,
    -- | The ID of the resource (for example., sg-xxxxxx).
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemediationException' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expirationTime', 'remediationException_expirationTime' - The time when the remediation exception will be deleted.
--
-- 'message', 'remediationException_message' - An explanation of an remediation exception.
--
-- 'configRuleName', 'remediationException_configRuleName' - The name of the AWS Config rule.
--
-- 'resourceType', 'remediationException_resourceType' - The type of a resource.
--
-- 'resourceId', 'remediationException_resourceId' - The ID of the resource (for example., sg-xxxxxx).
newRemediationException ::
  -- | 'configRuleName'
  Prelude.Text ->
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  RemediationException
newRemediationException
  pConfigRuleName_
  pResourceType_
  pResourceId_ =
    RemediationException'
      { expirationTime =
          Prelude.Nothing,
        message = Prelude.Nothing,
        configRuleName = pConfigRuleName_,
        resourceType = pResourceType_,
        resourceId = pResourceId_
      }

-- | The time when the remediation exception will be deleted.
remediationException_expirationTime :: Lens.Lens' RemediationException (Prelude.Maybe Prelude.UTCTime)
remediationException_expirationTime = Lens.lens (\RemediationException' {expirationTime} -> expirationTime) (\s@RemediationException' {} a -> s {expirationTime = a} :: RemediationException) Prelude.. Lens.mapping Prelude._Time

-- | An explanation of an remediation exception.
remediationException_message :: Lens.Lens' RemediationException (Prelude.Maybe Prelude.Text)
remediationException_message = Lens.lens (\RemediationException' {message} -> message) (\s@RemediationException' {} a -> s {message = a} :: RemediationException)

-- | The name of the AWS Config rule.
remediationException_configRuleName :: Lens.Lens' RemediationException Prelude.Text
remediationException_configRuleName = Lens.lens (\RemediationException' {configRuleName} -> configRuleName) (\s@RemediationException' {} a -> s {configRuleName = a} :: RemediationException)

-- | The type of a resource.
remediationException_resourceType :: Lens.Lens' RemediationException Prelude.Text
remediationException_resourceType = Lens.lens (\RemediationException' {resourceType} -> resourceType) (\s@RemediationException' {} a -> s {resourceType = a} :: RemediationException)

-- | The ID of the resource (for example., sg-xxxxxx).
remediationException_resourceId :: Lens.Lens' RemediationException Prelude.Text
remediationException_resourceId = Lens.lens (\RemediationException' {resourceId} -> resourceId) (\s@RemediationException' {} a -> s {resourceId = a} :: RemediationException)

instance Prelude.FromJSON RemediationException where
  parseJSON =
    Prelude.withObject
      "RemediationException"
      ( \x ->
          RemediationException'
            Prelude.<$> (x Prelude..:? "ExpirationTime")
            Prelude.<*> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..: "ConfigRuleName")
            Prelude.<*> (x Prelude..: "ResourceType")
            Prelude.<*> (x Prelude..: "ResourceId")
      )

instance Prelude.Hashable RemediationException

instance Prelude.NFData RemediationException
