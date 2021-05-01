{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.CreateTrial
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon SageMaker /trial/. A trial is a set of steps called
-- /trial components/ that produce a machine learning model. A trial is
-- part of a single Amazon SageMaker /experiment/.
--
-- When you use Amazon SageMaker Studio or the Amazon SageMaker Python SDK,
-- all experiments, trials, and trial components are automatically tracked,
-- logged, and indexed. When you use the AWS SDK for Python (Boto), you
-- must use the logging APIs provided by the SDK.
--
-- You can add tags to a trial and then use the Search API to search for
-- the tags.
--
-- To get a list of all your trials, call the ListTrials API. To view a
-- trial\'s properties, call the DescribeTrial API. To create a trial
-- component, call the CreateTrialComponent API.
module Network.AWS.SageMaker.CreateTrial
  ( -- * Creating a Request
    CreateTrial (..),
    newCreateTrial,

    -- * Request Lenses
    createTrial_metadataProperties,
    createTrial_tags,
    createTrial_displayName,
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateTrial' smart constructor.
data CreateTrial = CreateTrial'
  { metadataProperties :: Prelude.Maybe MetadataProperties,
    -- | A list of tags to associate with the trial. You can use Search API to
    -- search on the tags.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the trial as displayed. The name doesn\'t need to be unique.
    -- If @DisplayName@ isn\'t specified, @TrialName@ is displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The name of the trial. The name must be unique in your AWS account and
    -- is not case-sensitive.
    trialName :: Prelude.Text,
    -- | The name of the experiment to associate the trial with.
    experimentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateTrial' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadataProperties', 'createTrial_metadataProperties' - Undocumented member.
--
-- 'tags', 'createTrial_tags' - A list of tags to associate with the trial. You can use Search API to
-- search on the tags.
--
-- 'displayName', 'createTrial_displayName' - The name of the trial as displayed. The name doesn\'t need to be unique.
-- If @DisplayName@ isn\'t specified, @TrialName@ is displayed.
--
-- 'trialName', 'createTrial_trialName' - The name of the trial. The name must be unique in your AWS account and
-- is not case-sensitive.
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
    { metadataProperties = Prelude.Nothing,
      tags = Prelude.Nothing,
      displayName = Prelude.Nothing,
      trialName = pTrialName_,
      experimentName = pExperimentName_
    }

-- | Undocumented member.
createTrial_metadataProperties :: Lens.Lens' CreateTrial (Prelude.Maybe MetadataProperties)
createTrial_metadataProperties = Lens.lens (\CreateTrial' {metadataProperties} -> metadataProperties) (\s@CreateTrial' {} a -> s {metadataProperties = a} :: CreateTrial)

-- | A list of tags to associate with the trial. You can use Search API to
-- search on the tags.
createTrial_tags :: Lens.Lens' CreateTrial (Prelude.Maybe [Tag])
createTrial_tags = Lens.lens (\CreateTrial' {tags} -> tags) (\s@CreateTrial' {} a -> s {tags = a} :: CreateTrial) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the trial as displayed. The name doesn\'t need to be unique.
-- If @DisplayName@ isn\'t specified, @TrialName@ is displayed.
createTrial_displayName :: Lens.Lens' CreateTrial (Prelude.Maybe Prelude.Text)
createTrial_displayName = Lens.lens (\CreateTrial' {displayName} -> displayName) (\s@CreateTrial' {} a -> s {displayName = a} :: CreateTrial)

-- | The name of the trial. The name must be unique in your AWS account and
-- is not case-sensitive.
createTrial_trialName :: Lens.Lens' CreateTrial Prelude.Text
createTrial_trialName = Lens.lens (\CreateTrial' {trialName} -> trialName) (\s@CreateTrial' {} a -> s {trialName = a} :: CreateTrial)

-- | The name of the experiment to associate the trial with.
createTrial_experimentName :: Lens.Lens' CreateTrial Prelude.Text
createTrial_experimentName = Lens.lens (\CreateTrial' {experimentName} -> experimentName) (\s@CreateTrial' {} a -> s {experimentName = a} :: CreateTrial)

instance Prelude.AWSRequest CreateTrial where
  type Rs CreateTrial = CreateTrialResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTrialResponse'
            Prelude.<$> (x Prelude..?> "TrialArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTrial

instance Prelude.NFData CreateTrial

instance Prelude.ToHeaders CreateTrial where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.CreateTrial" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateTrial where
  toJSON CreateTrial' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("MetadataProperties" Prelude..=)
              Prelude.<$> metadataProperties,
            ("Tags" Prelude..=) Prelude.<$> tags,
            ("DisplayName" Prelude..=) Prelude.<$> displayName,
            Prelude.Just ("TrialName" Prelude..= trialName),
            Prelude.Just
              ("ExperimentName" Prelude..= experimentName)
          ]
      )

instance Prelude.ToPath CreateTrial where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateTrial where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTrialResponse' smart constructor.
data CreateTrialResponse = CreateTrialResponse'
  { -- | The Amazon Resource Name (ARN) of the trial.
    trialArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData CreateTrialResponse
