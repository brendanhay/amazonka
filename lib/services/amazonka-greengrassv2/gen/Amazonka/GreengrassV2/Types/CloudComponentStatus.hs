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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.CloudComponentStatus where

import qualified Amazonka.Core as Core
import Amazonka.GreengrassV2.Types.CloudComponentState
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the status of a component in the IoT Greengrass service.
--
-- /See:/ 'newCloudComponentStatus' smart constructor.
data CloudComponentStatus = CloudComponentStatus'
  { -- | A message that communicates details, such as errors, about the status of
    -- the component.
    message :: Prelude.Maybe Prelude.Text,
    -- | A dictionary of errors that communicate why the component is in an error
    -- state. For example, if IoT Greengrass can\'t access an artifact for the
    -- component, then @errors@ contains the artifact\'s URI as a key, and the
    -- error message as the value for that key.
    errors :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The state of the component.
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
-- the component.
--
-- 'errors', 'cloudComponentStatus_errors' - A dictionary of errors that communicate why the component is in an error
-- state. For example, if IoT Greengrass can\'t access an artifact for the
-- component, then @errors@ contains the artifact\'s URI as a key, and the
-- error message as the value for that key.
--
-- 'componentState', 'cloudComponentStatus_componentState' - The state of the component.
newCloudComponentStatus ::
  CloudComponentStatus
newCloudComponentStatus =
  CloudComponentStatus'
    { message = Prelude.Nothing,
      errors = Prelude.Nothing,
      componentState = Prelude.Nothing
    }

-- | A message that communicates details, such as errors, about the status of
-- the component.
cloudComponentStatus_message :: Lens.Lens' CloudComponentStatus (Prelude.Maybe Prelude.Text)
cloudComponentStatus_message = Lens.lens (\CloudComponentStatus' {message} -> message) (\s@CloudComponentStatus' {} a -> s {message = a} :: CloudComponentStatus)

-- | A dictionary of errors that communicate why the component is in an error
-- state. For example, if IoT Greengrass can\'t access an artifact for the
-- component, then @errors@ contains the artifact\'s URI as a key, and the
-- error message as the value for that key.
cloudComponentStatus_errors :: Lens.Lens' CloudComponentStatus (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
cloudComponentStatus_errors = Lens.lens (\CloudComponentStatus' {errors} -> errors) (\s@CloudComponentStatus' {} a -> s {errors = a} :: CloudComponentStatus) Prelude.. Lens.mapping Lens.coerced

-- | The state of the component.
cloudComponentStatus_componentState :: Lens.Lens' CloudComponentStatus (Prelude.Maybe CloudComponentState)
cloudComponentStatus_componentState = Lens.lens (\CloudComponentStatus' {componentState} -> componentState) (\s@CloudComponentStatus' {} a -> s {componentState = a} :: CloudComponentStatus)

instance Core.FromJSON CloudComponentStatus where
  parseJSON =
    Core.withObject
      "CloudComponentStatus"
      ( \x ->
          CloudComponentStatus'
            Prelude.<$> (x Core..:? "message")
            Prelude.<*> (x Core..:? "errors" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "componentState")
      )

instance Prelude.Hashable CloudComponentStatus where
  hashWithSalt _salt CloudComponentStatus' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` errors
      `Prelude.hashWithSalt` componentState

instance Prelude.NFData CloudComponentStatus where
  rnf CloudComponentStatus' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf componentState
