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
-- Module      : Amazonka.SageMaker.CreateTrial
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an SageMaker /trial/. A trial is a set of steps called /trial
-- components/ that produce a machine learning model. A trial is part of a
-- single SageMaker /experiment/.
--
-- When you use SageMaker Studio or the SageMaker Python SDK, all
-- experiments, trials, and trial components are automatically tracked,
-- logged, and indexed. When you use the Amazon Web Services SDK for Python
-- (Boto), you must use the logging APIs provided by the SDK.
--
-- You can add tags to a trial and then use the Search API to search for
-- the tags.
--
-- To get a list of all your trials, call the ListTrials API. To view a
-- trial\'s properties, call the DescribeTrial API. To create a trial
-- component, call the CreateTrialComponent API.
module Amazonka.SageMaker.CreateTrial
  ( -- * Creating a Request
    CreateTrial (..),
    newCreateTrial,

    -- * Request Lenses
    createTrial_displayName,
    createTrial_metadataProperties,
    createTrial_tags,
    createTrial_trialName,
    createTrial_experimentName,

    -- * Destructuring the Response
    CreateTrialResponse (..),
    newCreateTrialResponse,

    -- * Response Lenses
    createTrialResponse_trialArn,
    createTrialResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateTrial' smart constructor.
data CreateTrial = CreateTrial'
  { -- | The name of the trial as displayed. The name doesn\'t need to be unique.
    -- If @DisplayName@ isn\'t specified, @TrialName@ is displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    metadataProperties :: Prelude.Maybe MetadataProperties,
    -- | A list of tags to associate with the trial. You can use Search API to
    -- search on the tags.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the trial. The name must be unique in your Amazon Web
    -- Services account and is not case-sensitive.
    trialName :: Prelude.Text,
    -- | The name of the experiment to associate the trial with.
    experimentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTrial' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'createTrial_displayName' - The name of the trial as displayed. The name doesn\'t need to be unique.
-- If @DisplayName@ isn\'t specified, @TrialName@ is displayed.
--
-- 'metadataProperties', 'createTrial_metadataProperties' - Undocumented member.
--
-- 'tags', 'createTrial_tags' - A list of tags to associate with the trial. You can use Search API to
-- search on the tags.
--
-- 'trialName', 'createTrial_trialName' - The name of the trial. The name must be unique in your Amazon Web
-- Services account and is not case-sensitive.
--
-- 'experimentName', 'createTrial_experimentName' - The name of the experiment to associate the trial with.
newCreateTrial ::
  -- | 'trialName'
  Prelude.Text ->
  -- | 'experimentName'
  Prelude.Text ->
  CreateTrial
newCreateTrial pTrialName_ pExperimentName_ =
  CreateTrial'
    { displayName = Prelude.Nothing,
      metadataProperties = Prelude.Nothing,
      tags = Prelude.Nothing,
      trialName = pTrialName_,
      experimentName = pExperimentName_
    }

-- | The name of the trial as displayed. The name doesn\'t need to be unique.
-- If @DisplayName@ isn\'t specified, @TrialName@ is displayed.
createTrial_displayName :: Lens.Lens' CreateTrial (Prelude.Maybe Prelude.Text)
createTrial_displayName = Lens.lens (\CreateTrial' {displayName} -> displayName) (\s@CreateTrial' {} a -> s {displayName = a} :: CreateTrial)

-- | Undocumented member.
createTrial_metadataProperties :: Lens.Lens' CreateTrial (Prelude.Maybe MetadataProperties)
createTrial_metadataProperties = Lens.lens (\CreateTrial' {metadataProperties} -> metadataProperties) (\s@CreateTrial' {} a -> s {metadataProperties = a} :: CreateTrial)

-- | A list of tags to associate with the trial. You can use Search API to
-- search on the tags.
createTrial_tags :: Lens.Lens' CreateTrial (Prelude.Maybe [Tag])
createTrial_tags = Lens.lens (\CreateTrial' {tags} -> tags) (\s@CreateTrial' {} a -> s {tags = a} :: CreateTrial) Prelude.. Lens.mapping Lens.coerced

-- | The name of the trial. The name must be unique in your Amazon Web
-- Services account and is not case-sensitive.
createTrial_trialName :: Lens.Lens' CreateTrial Prelude.Text
createTrial_trialName = Lens.lens (\CreateTrial' {trialName} -> trialName) (\s@CreateTrial' {} a -> s {trialName = a} :: CreateTrial)

-- | The name of the experiment to associate the trial with.
createTrial_experimentName :: Lens.Lens' CreateTrial Prelude.Text
createTrial_experimentName = Lens.lens (\CreateTrial' {experimentName} -> experimentName) (\s@CreateTrial' {} a -> s {experimentName = a} :: CreateTrial)

instance Core.AWSRequest CreateTrial where
  type AWSResponse CreateTrial = CreateTrialResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTrialResponse'
            Prelude.<$> (x Data..?> "TrialArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTrial where
  hashWithSalt _salt CreateTrial' {..} =
    _salt
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` metadataProperties
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` trialName
      `Prelude.hashWithSalt` experimentName

instance Prelude.NFData CreateTrial where
  rnf CreateTrial' {..} =
    Prelude.rnf displayName `Prelude.seq`
      Prelude.rnf metadataProperties `Prelude.seq`
        Prelude.rnf tags `Prelude.seq`
          Prelude.rnf trialName `Prelude.seq`
            Prelude.rnf experimentName

instance Data.ToHeaders CreateTrial where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.CreateTrial" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTrial where
  toJSON CreateTrial' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DisplayName" Data..=) Prelude.<$> displayName,
            ("MetadataProperties" Data..=)
              Prelude.<$> metadataProperties,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("TrialName" Data..= trialName),
            Prelude.Just
              ("ExperimentName" Data..= experimentName)
          ]
      )

instance Data.ToPath CreateTrial where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateTrial where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTrialResponse' smart constructor.
data CreateTrialResponse = CreateTrialResponse'
  { -- | The Amazon Resource Name (ARN) of the trial.
    trialArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTrialResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trialArn', 'createTrialResponse_trialArn' - The Amazon Resource Name (ARN) of the trial.
--
-- 'httpStatus', 'createTrialResponse_httpStatus' - The response's http status code.
newCreateTrialResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTrialResponse
newCreateTrialResponse pHttpStatus_ =
  CreateTrialResponse'
    { trialArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the trial.
createTrialResponse_trialArn :: Lens.Lens' CreateTrialResponse (Prelude.Maybe Prelude.Text)
createTrialResponse_trialArn = Lens.lens (\CreateTrialResponse' {trialArn} -> trialArn) (\s@CreateTrialResponse' {} a -> s {trialArn = a} :: CreateTrialResponse)

-- | The response's http status code.
createTrialResponse_httpStatus :: Lens.Lens' CreateTrialResponse Prelude.Int
createTrialResponse_httpStatus = Lens.lens (\CreateTrialResponse' {httpStatus} -> httpStatus) (\s@CreateTrialResponse' {} a -> s {httpStatus = a} :: CreateTrialResponse)

instance Prelude.NFData CreateTrialResponse where
  rnf CreateTrialResponse' {..} =
    Prelude.rnf trialArn `Prelude.seq`
      Prelude.rnf httpStatus
