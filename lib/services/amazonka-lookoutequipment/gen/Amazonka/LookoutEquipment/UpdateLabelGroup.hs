{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LookoutEquipment.UpdateLabelGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the label group.
module Amazonka.LookoutEquipment.UpdateLabelGroup
  ( -- * Creating a Request
    UpdateLabelGroup (..),
    newUpdateLabelGroup,

    -- * Request Lenses
    updateLabelGroup_faultCodes,
    updateLabelGroup_labelGroupName,

    -- * Destructuring the Response
    UpdateLabelGroupResponse (..),
    newUpdateLabelGroupResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutEquipment.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateLabelGroup' smart constructor.
data UpdateLabelGroup = UpdateLabelGroup'
  { -- | Updates the code indicating the type of anomaly associated with the
    -- label.
    --
    -- Data in this field will be retained for service usage. Follow best
    -- practices for the security of your data.
    faultCodes :: Prelude.Maybe [Prelude.Text],
    -- | The name of the label group to be updated.
    labelGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLabelGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faultCodes', 'updateLabelGroup_faultCodes' - Updates the code indicating the type of anomaly associated with the
-- label.
--
-- Data in this field will be retained for service usage. Follow best
-- practices for the security of your data.
--
-- 'labelGroupName', 'updateLabelGroup_labelGroupName' - The name of the label group to be updated.
newUpdateLabelGroup ::
  -- | 'labelGroupName'
  Prelude.Text ->
  UpdateLabelGroup
newUpdateLabelGroup pLabelGroupName_ =
  UpdateLabelGroup'
    { faultCodes = Prelude.Nothing,
      labelGroupName = pLabelGroupName_
    }

-- | Updates the code indicating the type of anomaly associated with the
-- label.
--
-- Data in this field will be retained for service usage. Follow best
-- practices for the security of your data.
updateLabelGroup_faultCodes :: Lens.Lens' UpdateLabelGroup (Prelude.Maybe [Prelude.Text])
updateLabelGroup_faultCodes = Lens.lens (\UpdateLabelGroup' {faultCodes} -> faultCodes) (\s@UpdateLabelGroup' {} a -> s {faultCodes = a} :: UpdateLabelGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the label group to be updated.
updateLabelGroup_labelGroupName :: Lens.Lens' UpdateLabelGroup Prelude.Text
updateLabelGroup_labelGroupName = Lens.lens (\UpdateLabelGroup' {labelGroupName} -> labelGroupName) (\s@UpdateLabelGroup' {} a -> s {labelGroupName = a} :: UpdateLabelGroup)

instance Core.AWSRequest UpdateLabelGroup where
  type
    AWSResponse UpdateLabelGroup =
      UpdateLabelGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateLabelGroupResponse'

instance Prelude.Hashable UpdateLabelGroup where
  hashWithSalt _salt UpdateLabelGroup' {..} =
    _salt `Prelude.hashWithSalt` faultCodes
      `Prelude.hashWithSalt` labelGroupName

instance Prelude.NFData UpdateLabelGroup where
  rnf UpdateLabelGroup' {..} =
    Prelude.rnf faultCodes
      `Prelude.seq` Prelude.rnf labelGroupName

instance Core.ToHeaders UpdateLabelGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLookoutEquipmentFrontendService.UpdateLabelGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateLabelGroup where
  toJSON UpdateLabelGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FaultCodes" Core..=) Prelude.<$> faultCodes,
            Prelude.Just
              ("LabelGroupName" Core..= labelGroupName)
          ]
      )

instance Core.ToPath UpdateLabelGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateLabelGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateLabelGroupResponse' smart constructor.
data UpdateLabelGroupResponse = UpdateLabelGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLabelGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateLabelGroupResponse ::
  UpdateLabelGroupResponse
newUpdateLabelGroupResponse =
  UpdateLabelGroupResponse'

instance Prelude.NFData UpdateLabelGroupResponse where
  rnf _ = ()
