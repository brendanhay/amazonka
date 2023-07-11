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
-- Module      : Amazonka.SSM.LabelParameterVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A parameter label is a user-defined alias to help you manage different
-- versions of a parameter. When you modify a parameter, Amazon Web
-- Services Systems Manager automatically saves a new version and
-- increments the version number by one. A label can help you remember the
-- purpose of a parameter when there are multiple versions.
--
-- Parameter labels have the following requirements and restrictions.
--
-- -   A version of a parameter can have a maximum of 10 labels.
--
-- -   You can\'t attach the same label to different versions of the same
--     parameter. For example, if version 1 has the label Production, then
--     you can\'t attach Production to version 2.
--
-- -   You can move a label from one version of a parameter to another.
--
-- -   You can\'t create a label when you create a new parameter. You must
--     attach a label to a specific version of a parameter.
--
-- -   If you no longer want to use a parameter label, then you can either
--     delete it or move it to a different version of a parameter.
--
-- -   A label can have a maximum of 100 characters.
--
-- -   Labels can contain letters (case sensitive), numbers, periods (.),
--     hyphens (-), or underscores (_).
--
-- -   Labels can\'t begin with a number, \"@aws@\" or \"@ssm@\" (not case
--     sensitive). If a label fails to meet these requirements, then the
--     label isn\'t associated with a parameter and the system displays it
--     in the list of InvalidLabels.
module Amazonka.SSM.LabelParameterVersion
  ( -- * Creating a Request
    LabelParameterVersion (..),
    newLabelParameterVersion,

    -- * Request Lenses
    labelParameterVersion_parameterVersion,
    labelParameterVersion_name,
    labelParameterVersion_labels,

    -- * Destructuring the Response
    LabelParameterVersionResponse (..),
    newLabelParameterVersionResponse,

    -- * Response Lenses
    labelParameterVersionResponse_invalidLabels,
    labelParameterVersionResponse_parameterVersion,
    labelParameterVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newLabelParameterVersion' smart constructor.
data LabelParameterVersion = LabelParameterVersion'
  { -- | The specific version of the parameter on which you want to attach one or
    -- more labels. If no version is specified, the system attaches the label
    -- to the latest version.
    parameterVersion :: Prelude.Maybe Prelude.Integer,
    -- | The parameter name on which you want to attach one or more labels.
    name :: Prelude.Text,
    -- | One or more labels to attach to the specified parameter version.
    labels :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelParameterVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterVersion', 'labelParameterVersion_parameterVersion' - The specific version of the parameter on which you want to attach one or
-- more labels. If no version is specified, the system attaches the label
-- to the latest version.
--
-- 'name', 'labelParameterVersion_name' - The parameter name on which you want to attach one or more labels.
--
-- 'labels', 'labelParameterVersion_labels' - One or more labels to attach to the specified parameter version.
newLabelParameterVersion ::
  -- | 'name'
  Prelude.Text ->
  -- | 'labels'
  Prelude.NonEmpty Prelude.Text ->
  LabelParameterVersion
newLabelParameterVersion pName_ pLabels_ =
  LabelParameterVersion'
    { parameterVersion =
        Prelude.Nothing,
      name = pName_,
      labels = Lens.coerced Lens.# pLabels_
    }

-- | The specific version of the parameter on which you want to attach one or
-- more labels. If no version is specified, the system attaches the label
-- to the latest version.
labelParameterVersion_parameterVersion :: Lens.Lens' LabelParameterVersion (Prelude.Maybe Prelude.Integer)
labelParameterVersion_parameterVersion = Lens.lens (\LabelParameterVersion' {parameterVersion} -> parameterVersion) (\s@LabelParameterVersion' {} a -> s {parameterVersion = a} :: LabelParameterVersion)

-- | The parameter name on which you want to attach one or more labels.
labelParameterVersion_name :: Lens.Lens' LabelParameterVersion Prelude.Text
labelParameterVersion_name = Lens.lens (\LabelParameterVersion' {name} -> name) (\s@LabelParameterVersion' {} a -> s {name = a} :: LabelParameterVersion)

-- | One or more labels to attach to the specified parameter version.
labelParameterVersion_labels :: Lens.Lens' LabelParameterVersion (Prelude.NonEmpty Prelude.Text)
labelParameterVersion_labels = Lens.lens (\LabelParameterVersion' {labels} -> labels) (\s@LabelParameterVersion' {} a -> s {labels = a} :: LabelParameterVersion) Prelude.. Lens.coerced

instance Core.AWSRequest LabelParameterVersion where
  type
    AWSResponse LabelParameterVersion =
      LabelParameterVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          LabelParameterVersionResponse'
            Prelude.<$> (x Data..?> "InvalidLabels")
            Prelude.<*> (x Data..?> "ParameterVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable LabelParameterVersion where
  hashWithSalt _salt LabelParameterVersion' {..} =
    _salt
      `Prelude.hashWithSalt` parameterVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` labels

instance Prelude.NFData LabelParameterVersion where
  rnf LabelParameterVersion' {..} =
    Prelude.rnf parameterVersion
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf labels

instance Data.ToHeaders LabelParameterVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.LabelParameterVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON LabelParameterVersion where
  toJSON LabelParameterVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ParameterVersion" Data..=)
              Prelude.<$> parameterVersion,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Labels" Data..= labels)
          ]
      )

instance Data.ToPath LabelParameterVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery LabelParameterVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newLabelParameterVersionResponse' smart constructor.
data LabelParameterVersionResponse = LabelParameterVersionResponse'
  { -- | The label doesn\'t meet the requirements. For information about
    -- parameter label requirements, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-labels.html Labeling parameters>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    invalidLabels :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The version of the parameter that has been labeled.
    parameterVersion :: Prelude.Maybe Prelude.Integer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelParameterVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invalidLabels', 'labelParameterVersionResponse_invalidLabels' - The label doesn\'t meet the requirements. For information about
-- parameter label requirements, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-labels.html Labeling parameters>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'parameterVersion', 'labelParameterVersionResponse_parameterVersion' - The version of the parameter that has been labeled.
--
-- 'httpStatus', 'labelParameterVersionResponse_httpStatus' - The response's http status code.
newLabelParameterVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  LabelParameterVersionResponse
newLabelParameterVersionResponse pHttpStatus_ =
  LabelParameterVersionResponse'
    { invalidLabels =
        Prelude.Nothing,
      parameterVersion = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The label doesn\'t meet the requirements. For information about
-- parameter label requirements, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-labels.html Labeling parameters>
-- in the /Amazon Web Services Systems Manager User Guide/.
labelParameterVersionResponse_invalidLabels :: Lens.Lens' LabelParameterVersionResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
labelParameterVersionResponse_invalidLabels = Lens.lens (\LabelParameterVersionResponse' {invalidLabels} -> invalidLabels) (\s@LabelParameterVersionResponse' {} a -> s {invalidLabels = a} :: LabelParameterVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The version of the parameter that has been labeled.
labelParameterVersionResponse_parameterVersion :: Lens.Lens' LabelParameterVersionResponse (Prelude.Maybe Prelude.Integer)
labelParameterVersionResponse_parameterVersion = Lens.lens (\LabelParameterVersionResponse' {parameterVersion} -> parameterVersion) (\s@LabelParameterVersionResponse' {} a -> s {parameterVersion = a} :: LabelParameterVersionResponse)

-- | The response's http status code.
labelParameterVersionResponse_httpStatus :: Lens.Lens' LabelParameterVersionResponse Prelude.Int
labelParameterVersionResponse_httpStatus = Lens.lens (\LabelParameterVersionResponse' {httpStatus} -> httpStatus) (\s@LabelParameterVersionResponse' {} a -> s {httpStatus = a} :: LabelParameterVersionResponse)

instance Prelude.NFData LabelParameterVersionResponse where
  rnf LabelParameterVersionResponse' {..} =
    Prelude.rnf invalidLabels
      `Prelude.seq` Prelude.rnf parameterVersion
      `Prelude.seq` Prelude.rnf httpStatus
