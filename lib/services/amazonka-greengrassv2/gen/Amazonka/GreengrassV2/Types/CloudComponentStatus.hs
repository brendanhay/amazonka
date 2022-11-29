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
-- Module      : Amazonka.GreengrassV2.Types.CloudComponentStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.CloudComponentStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GreengrassV2.Types.CloudComponentState
import Amazonka.GreengrassV2.Types.VendorGuidance
import qualified Amazonka.Prelude as Prelude

-- | Contains the status of a component version in the IoT Greengrass
-- service.
--
-- /See:/ 'newCloudComponentStatus' smart constructor.
data CloudComponentStatus = CloudComponentStatus'
  { -- | A message that communicates details, such as errors, about the status of
    -- the component version.
    message :: Prelude.Maybe Prelude.Text,
    -- | The vendor guidance state for the component version. This state
    -- indicates whether the component version has any issues that you should
    -- consider before you deploy it. The vendor guidance state can be:
    --
    -- -   @ACTIVE@ – This component version is available and recommended for
    --     use.
    --
    -- -   @DISCONTINUED@ – This component version has been discontinued by its
    --     publisher. You can deploy this component version, but we recommend
    --     that you use a different version of this component.
    --
    -- -   @DELETED@ – This component version has been deleted by its
    --     publisher, so you can\'t deploy it. If you have any existing
    --     deployments that specify this component version, those deployments
    --     will fail.
    vendorGuidance :: Prelude.Maybe VendorGuidance,
    -- | A message that communicates details about the vendor guidance state of
    -- the component version. This message communicates why a component version
    -- is discontinued or deleted.
    vendorGuidanceMessage :: Prelude.Maybe Prelude.Text,
    -- | A dictionary of errors that communicate why the component version is in
    -- an error state. For example, if IoT Greengrass can\'t access an artifact
    -- for the component version, then @errors@ contains the artifact\'s URI as
    -- a key, and the error message as the value for that key.
    errors :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The state of the component version.
    componentState :: Prelude.Maybe CloudComponentState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudComponentStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'cloudComponentStatus_message' - A message that communicates details, such as errors, about the status of
-- the component version.
--
-- 'vendorGuidance', 'cloudComponentStatus_vendorGuidance' - The vendor guidance state for the component version. This state
-- indicates whether the component version has any issues that you should
-- consider before you deploy it. The vendor guidance state can be:
--
-- -   @ACTIVE@ – This component version is available and recommended for
--     use.
--
-- -   @DISCONTINUED@ – This component version has been discontinued by its
--     publisher. You can deploy this component version, but we recommend
--     that you use a different version of this component.
--
-- -   @DELETED@ – This component version has been deleted by its
--     publisher, so you can\'t deploy it. If you have any existing
--     deployments that specify this component version, those deployments
--     will fail.
--
-- 'vendorGuidanceMessage', 'cloudComponentStatus_vendorGuidanceMessage' - A message that communicates details about the vendor guidance state of
-- the component version. This message communicates why a component version
-- is discontinued or deleted.
--
-- 'errors', 'cloudComponentStatus_errors' - A dictionary of errors that communicate why the component version is in
-- an error state. For example, if IoT Greengrass can\'t access an artifact
-- for the component version, then @errors@ contains the artifact\'s URI as
-- a key, and the error message as the value for that key.
--
-- 'componentState', 'cloudComponentStatus_componentState' - The state of the component version.
newCloudComponentStatus ::
  CloudComponentStatus
newCloudComponentStatus =
  CloudComponentStatus'
    { message = Prelude.Nothing,
      vendorGuidance = Prelude.Nothing,
      vendorGuidanceMessage = Prelude.Nothing,
      errors = Prelude.Nothing,
      componentState = Prelude.Nothing
    }

-- | A message that communicates details, such as errors, about the status of
-- the component version.
cloudComponentStatus_message :: Lens.Lens' CloudComponentStatus (Prelude.Maybe Prelude.Text)
cloudComponentStatus_message = Lens.lens (\CloudComponentStatus' {message} -> message) (\s@CloudComponentStatus' {} a -> s {message = a} :: CloudComponentStatus)

-- | The vendor guidance state for the component version. This state
-- indicates whether the component version has any issues that you should
-- consider before you deploy it. The vendor guidance state can be:
--
-- -   @ACTIVE@ – This component version is available and recommended for
--     use.
--
-- -   @DISCONTINUED@ – This component version has been discontinued by its
--     publisher. You can deploy this component version, but we recommend
--     that you use a different version of this component.
--
-- -   @DELETED@ – This component version has been deleted by its
--     publisher, so you can\'t deploy it. If you have any existing
--     deployments that specify this component version, those deployments
--     will fail.
cloudComponentStatus_vendorGuidance :: Lens.Lens' CloudComponentStatus (Prelude.Maybe VendorGuidance)
cloudComponentStatus_vendorGuidance = Lens.lens (\CloudComponentStatus' {vendorGuidance} -> vendorGuidance) (\s@CloudComponentStatus' {} a -> s {vendorGuidance = a} :: CloudComponentStatus)

-- | A message that communicates details about the vendor guidance state of
-- the component version. This message communicates why a component version
-- is discontinued or deleted.
cloudComponentStatus_vendorGuidanceMessage :: Lens.Lens' CloudComponentStatus (Prelude.Maybe Prelude.Text)
cloudComponentStatus_vendorGuidanceMessage = Lens.lens (\CloudComponentStatus' {vendorGuidanceMessage} -> vendorGuidanceMessage) (\s@CloudComponentStatus' {} a -> s {vendorGuidanceMessage = a} :: CloudComponentStatus)

-- | A dictionary of errors that communicate why the component version is in
-- an error state. For example, if IoT Greengrass can\'t access an artifact
-- for the component version, then @errors@ contains the artifact\'s URI as
-- a key, and the error message as the value for that key.
cloudComponentStatus_errors :: Lens.Lens' CloudComponentStatus (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
cloudComponentStatus_errors = Lens.lens (\CloudComponentStatus' {errors} -> errors) (\s@CloudComponentStatus' {} a -> s {errors = a} :: CloudComponentStatus) Prelude.. Lens.mapping Lens.coerced

-- | The state of the component version.
cloudComponentStatus_componentState :: Lens.Lens' CloudComponentStatus (Prelude.Maybe CloudComponentState)
cloudComponentStatus_componentState = Lens.lens (\CloudComponentStatus' {componentState} -> componentState) (\s@CloudComponentStatus' {} a -> s {componentState = a} :: CloudComponentStatus)

instance Core.FromJSON CloudComponentStatus where
  parseJSON =
    Core.withObject
      "CloudComponentStatus"
      ( \x ->
          CloudComponentStatus'
            Prelude.<$> (x Core..:? "message")
            Prelude.<*> (x Core..:? "vendorGuidance")
            Prelude.<*> (x Core..:? "vendorGuidanceMessage")
            Prelude.<*> (x Core..:? "errors" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "componentState")
      )

instance Prelude.Hashable CloudComponentStatus where
  hashWithSalt _salt CloudComponentStatus' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` vendorGuidance
      `Prelude.hashWithSalt` vendorGuidanceMessage
      `Prelude.hashWithSalt` errors
      `Prelude.hashWithSalt` componentState

instance Prelude.NFData CloudComponentStatus where
  rnf CloudComponentStatus' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf vendorGuidance
      `Prelude.seq` Prelude.rnf vendorGuidanceMessage
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf componentState
