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
-- Module      : Amazonka.SSM.UnlabelParameterVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove a label or labels from a parameter.
module Amazonka.SSM.UnlabelParameterVersion
  ( -- * Creating a Request
    UnlabelParameterVersion (..),
    newUnlabelParameterVersion,

    -- * Request Lenses
    unlabelParameterVersion_name,
    unlabelParameterVersion_parameterVersion,
    unlabelParameterVersion_labels,

    -- * Destructuring the Response
    UnlabelParameterVersionResponse (..),
    newUnlabelParameterVersionResponse,

    -- * Response Lenses
    unlabelParameterVersionResponse_invalidLabels,
    unlabelParameterVersionResponse_removedLabels,
    unlabelParameterVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newUnlabelParameterVersion' smart constructor.
data UnlabelParameterVersion = UnlabelParameterVersion'
  { -- | The name of the parameter from which you want to delete one or more
    -- labels.
    name :: Prelude.Text,
    -- | The specific version of the parameter which you want to delete one or
    -- more labels from. If it isn\'t present, the call will fail.
    parameterVersion :: Prelude.Integer,
    -- | One or more labels to delete from the specified parameter version.
    labels :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnlabelParameterVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'unlabelParameterVersion_name' - The name of the parameter from which you want to delete one or more
-- labels.
--
-- 'parameterVersion', 'unlabelParameterVersion_parameterVersion' - The specific version of the parameter which you want to delete one or
-- more labels from. If it isn\'t present, the call will fail.
--
-- 'labels', 'unlabelParameterVersion_labels' - One or more labels to delete from the specified parameter version.
newUnlabelParameterVersion ::
  -- | 'name'
  Prelude.Text ->
  -- | 'parameterVersion'
  Prelude.Integer ->
  -- | 'labels'
  Prelude.NonEmpty Prelude.Text ->
  UnlabelParameterVersion
newUnlabelParameterVersion
  pName_
  pParameterVersion_
  pLabels_ =
    UnlabelParameterVersion'
      { name = pName_,
        parameterVersion = pParameterVersion_,
        labels = Lens.coerced Lens.# pLabels_
      }

-- | The name of the parameter from which you want to delete one or more
-- labels.
unlabelParameterVersion_name :: Lens.Lens' UnlabelParameterVersion Prelude.Text
unlabelParameterVersion_name = Lens.lens (\UnlabelParameterVersion' {name} -> name) (\s@UnlabelParameterVersion' {} a -> s {name = a} :: UnlabelParameterVersion)

-- | The specific version of the parameter which you want to delete one or
-- more labels from. If it isn\'t present, the call will fail.
unlabelParameterVersion_parameterVersion :: Lens.Lens' UnlabelParameterVersion Prelude.Integer
unlabelParameterVersion_parameterVersion = Lens.lens (\UnlabelParameterVersion' {parameterVersion} -> parameterVersion) (\s@UnlabelParameterVersion' {} a -> s {parameterVersion = a} :: UnlabelParameterVersion)

-- | One or more labels to delete from the specified parameter version.
unlabelParameterVersion_labels :: Lens.Lens' UnlabelParameterVersion (Prelude.NonEmpty Prelude.Text)
unlabelParameterVersion_labels = Lens.lens (\UnlabelParameterVersion' {labels} -> labels) (\s@UnlabelParameterVersion' {} a -> s {labels = a} :: UnlabelParameterVersion) Prelude.. Lens.coerced

instance Core.AWSRequest UnlabelParameterVersion where
  type
    AWSResponse UnlabelParameterVersion =
      UnlabelParameterVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UnlabelParameterVersionResponse'
            Prelude.<$> (x Data..?> "InvalidLabels")
            Prelude.<*> (x Data..?> "RemovedLabels")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UnlabelParameterVersion where
  hashWithSalt _salt UnlabelParameterVersion' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` parameterVersion
      `Prelude.hashWithSalt` labels

instance Prelude.NFData UnlabelParameterVersion where
  rnf UnlabelParameterVersion' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf parameterVersion
      `Prelude.seq` Prelude.rnf labels

instance Data.ToHeaders UnlabelParameterVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.UnlabelParameterVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UnlabelParameterVersion where
  toJSON UnlabelParameterVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("ParameterVersion" Data..= parameterVersion),
            Prelude.Just ("Labels" Data..= labels)
          ]
      )

instance Data.ToPath UnlabelParameterVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery UnlabelParameterVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUnlabelParameterVersionResponse' smart constructor.
data UnlabelParameterVersionResponse = UnlabelParameterVersionResponse'
  { -- | The labels that aren\'t attached to the given parameter version.
    invalidLabels :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of all labels deleted from the parameter.
    removedLabels :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnlabelParameterVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invalidLabels', 'unlabelParameterVersionResponse_invalidLabels' - The labels that aren\'t attached to the given parameter version.
--
-- 'removedLabels', 'unlabelParameterVersionResponse_removedLabels' - A list of all labels deleted from the parameter.
--
-- 'httpStatus', 'unlabelParameterVersionResponse_httpStatus' - The response's http status code.
newUnlabelParameterVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UnlabelParameterVersionResponse
newUnlabelParameterVersionResponse pHttpStatus_ =
  UnlabelParameterVersionResponse'
    { invalidLabels =
        Prelude.Nothing,
      removedLabels = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The labels that aren\'t attached to the given parameter version.
unlabelParameterVersionResponse_invalidLabels :: Lens.Lens' UnlabelParameterVersionResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
unlabelParameterVersionResponse_invalidLabels = Lens.lens (\UnlabelParameterVersionResponse' {invalidLabels} -> invalidLabels) (\s@UnlabelParameterVersionResponse' {} a -> s {invalidLabels = a} :: UnlabelParameterVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of all labels deleted from the parameter.
unlabelParameterVersionResponse_removedLabels :: Lens.Lens' UnlabelParameterVersionResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
unlabelParameterVersionResponse_removedLabels = Lens.lens (\UnlabelParameterVersionResponse' {removedLabels} -> removedLabels) (\s@UnlabelParameterVersionResponse' {} a -> s {removedLabels = a} :: UnlabelParameterVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
unlabelParameterVersionResponse_httpStatus :: Lens.Lens' UnlabelParameterVersionResponse Prelude.Int
unlabelParameterVersionResponse_httpStatus = Lens.lens (\UnlabelParameterVersionResponse' {httpStatus} -> httpStatus) (\s@UnlabelParameterVersionResponse' {} a -> s {httpStatus = a} :: UnlabelParameterVersionResponse)

instance
  Prelude.NFData
    UnlabelParameterVersionResponse
  where
  rnf UnlabelParameterVersionResponse' {..} =
    Prelude.rnf invalidLabels
      `Prelude.seq` Prelude.rnf removedLabels
      `Prelude.seq` Prelude.rnf httpStatus
