{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.CreateQualificationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @CreateQualificationType@ operation creates a new Qualification type, which is represented by a @QualificationType@ data structure.
module Network.AWS.MechanicalTurk.CreateQualificationType
  ( -- * Creating a request
    CreateQualificationType (..),
    mkCreateQualificationType,

    -- ** Request lenses
    cqtTestDurationInSeconds,
    cqtQualificationTypeStatus,
    cqtAnswerKey,
    cqtTest,
    cqtName,
    cqtKeywords,
    cqtAutoGranted,
    cqtAutoGrantedValue,
    cqtDescription,
    cqtRetryDelayInSeconds,

    -- * Destructuring the response
    CreateQualificationTypeResponse (..),
    mkCreateQualificationTypeResponse,

    -- ** Response lenses
    cqtrsQualificationType,
    cqtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateQualificationType' smart constructor.
data CreateQualificationType = CreateQualificationType'
  { -- | The number of seconds the Worker has to complete the Qualification test, starting from the time the Worker requests the Qualification.
    testDurationInSeconds :: Lude.Maybe Lude.Integer,
    -- | The initial status of the Qualification type.
    --
    -- Constraints: Valid values are: Active | Inactive
    qualificationTypeStatus :: QualificationTypeStatus,
    -- | The answers to the Qualification test specified in the Test parameter, in the form of an AnswerKey data structure.
    --
    -- Constraints: Must not be longer than 65535 bytes.
    -- Constraints: None. If not specified, you must process Qualification requests manually.
    answerKey :: Lude.Maybe Lude.Text,
    -- | The questions for the Qualification test a Worker must answer correctly to obtain a Qualification of this type. If this parameter is specified, @TestDurationInSeconds@ must also be specified.
    --
    -- Constraints: Must not be longer than 65535 bytes. Must be a QuestionForm data structure. This parameter cannot be specified if AutoGranted is true.
    -- Constraints: None. If not specified, the Worker may request the Qualification without answering any questions.
    test :: Lude.Maybe Lude.Text,
    -- | The name you give to the Qualification type. The type name is used to represent the Qualification to Workers, and to find the type using a Qualification type search. It must be unique across all of your Qualification types.
    name :: Lude.Text,
    -- | One or more words or phrases that describe the Qualification type, separated by commas. The keywords of a type make the type easier to find during a search.
    keywords :: Lude.Maybe Lude.Text,
    -- | Specifies whether requests for the Qualification type are granted immediately, without prompting the Worker with a Qualification test.
    --
    -- Constraints: If the Test parameter is specified, this parameter cannot be true.
    autoGranted :: Lude.Maybe Lude.Bool,
    -- | The Qualification value to use for automatically granted Qualifications. This parameter is used only if the AutoGranted parameter is true.
    autoGrantedValue :: Lude.Maybe Lude.Int,
    -- | A long description for the Qualification type. On the Amazon Mechanical Turk website, the long description is displayed when a Worker examines a Qualification type.
    description :: Lude.Text,
    -- | The number of seconds that a Worker must wait after requesting a Qualification of the Qualification type before the worker can retry the Qualification request.
    --
    -- Constraints: None. If not specified, retries are disabled and Workers can request a Qualification of this type only once, even if the Worker has not been granted the Qualification. It is not possible to disable retries for a Qualification type after it has been created with retries enabled. If you want to disable retries, you must delete existing retry-enabled Qualification type and then create a new Qualification type with retries disabled.
    retryDelayInSeconds :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateQualificationType' with the minimum fields required to make a request.
--
-- * 'testDurationInSeconds' - The number of seconds the Worker has to complete the Qualification test, starting from the time the Worker requests the Qualification.
-- * 'qualificationTypeStatus' - The initial status of the Qualification type.
--
-- Constraints: Valid values are: Active | Inactive
-- * 'answerKey' - The answers to the Qualification test specified in the Test parameter, in the form of an AnswerKey data structure.
--
-- Constraints: Must not be longer than 65535 bytes.
-- Constraints: None. If not specified, you must process Qualification requests manually.
-- * 'test' - The questions for the Qualification test a Worker must answer correctly to obtain a Qualification of this type. If this parameter is specified, @TestDurationInSeconds@ must also be specified.
--
-- Constraints: Must not be longer than 65535 bytes. Must be a QuestionForm data structure. This parameter cannot be specified if AutoGranted is true.
-- Constraints: None. If not specified, the Worker may request the Qualification without answering any questions.
-- * 'name' - The name you give to the Qualification type. The type name is used to represent the Qualification to Workers, and to find the type using a Qualification type search. It must be unique across all of your Qualification types.
-- * 'keywords' - One or more words or phrases that describe the Qualification type, separated by commas. The keywords of a type make the type easier to find during a search.
-- * 'autoGranted' - Specifies whether requests for the Qualification type are granted immediately, without prompting the Worker with a Qualification test.
--
-- Constraints: If the Test parameter is specified, this parameter cannot be true.
-- * 'autoGrantedValue' - The Qualification value to use for automatically granted Qualifications. This parameter is used only if the AutoGranted parameter is true.
-- * 'description' - A long description for the Qualification type. On the Amazon Mechanical Turk website, the long description is displayed when a Worker examines a Qualification type.
-- * 'retryDelayInSeconds' - The number of seconds that a Worker must wait after requesting a Qualification of the Qualification type before the worker can retry the Qualification request.
--
-- Constraints: None. If not specified, retries are disabled and Workers can request a Qualification of this type only once, even if the Worker has not been granted the Qualification. It is not possible to disable retries for a Qualification type after it has been created with retries enabled. If you want to disable retries, you must delete existing retry-enabled Qualification type and then create a new Qualification type with retries disabled.
mkCreateQualificationType ::
  -- | 'qualificationTypeStatus'
  QualificationTypeStatus ->
  -- | 'name'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  CreateQualificationType
mkCreateQualificationType
  pQualificationTypeStatus_
  pName_
  pDescription_ =
    CreateQualificationType'
      { testDurationInSeconds = Lude.Nothing,
        qualificationTypeStatus = pQualificationTypeStatus_,
        answerKey = Lude.Nothing,
        test = Lude.Nothing,
        name = pName_,
        keywords = Lude.Nothing,
        autoGranted = Lude.Nothing,
        autoGrantedValue = Lude.Nothing,
        description = pDescription_,
        retryDelayInSeconds = Lude.Nothing
      }

-- | The number of seconds the Worker has to complete the Qualification test, starting from the time the Worker requests the Qualification.
--
-- /Note:/ Consider using 'testDurationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqtTestDurationInSeconds :: Lens.Lens' CreateQualificationType (Lude.Maybe Lude.Integer)
cqtTestDurationInSeconds = Lens.lens (testDurationInSeconds :: CreateQualificationType -> Lude.Maybe Lude.Integer) (\s a -> s {testDurationInSeconds = a} :: CreateQualificationType)
{-# DEPRECATED cqtTestDurationInSeconds "Use generic-lens or generic-optics with 'testDurationInSeconds' instead." #-}

-- | The initial status of the Qualification type.
--
-- Constraints: Valid values are: Active | Inactive
--
-- /Note:/ Consider using 'qualificationTypeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqtQualificationTypeStatus :: Lens.Lens' CreateQualificationType QualificationTypeStatus
cqtQualificationTypeStatus = Lens.lens (qualificationTypeStatus :: CreateQualificationType -> QualificationTypeStatus) (\s a -> s {qualificationTypeStatus = a} :: CreateQualificationType)
{-# DEPRECATED cqtQualificationTypeStatus "Use generic-lens or generic-optics with 'qualificationTypeStatus' instead." #-}

-- | The answers to the Qualification test specified in the Test parameter, in the form of an AnswerKey data structure.
--
-- Constraints: Must not be longer than 65535 bytes.
-- Constraints: None. If not specified, you must process Qualification requests manually.
--
-- /Note:/ Consider using 'answerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqtAnswerKey :: Lens.Lens' CreateQualificationType (Lude.Maybe Lude.Text)
cqtAnswerKey = Lens.lens (answerKey :: CreateQualificationType -> Lude.Maybe Lude.Text) (\s a -> s {answerKey = a} :: CreateQualificationType)
{-# DEPRECATED cqtAnswerKey "Use generic-lens or generic-optics with 'answerKey' instead." #-}

-- | The questions for the Qualification test a Worker must answer correctly to obtain a Qualification of this type. If this parameter is specified, @TestDurationInSeconds@ must also be specified.
--
-- Constraints: Must not be longer than 65535 bytes. Must be a QuestionForm data structure. This parameter cannot be specified if AutoGranted is true.
-- Constraints: None. If not specified, the Worker may request the Qualification without answering any questions.
--
-- /Note:/ Consider using 'test' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqtTest :: Lens.Lens' CreateQualificationType (Lude.Maybe Lude.Text)
cqtTest = Lens.lens (test :: CreateQualificationType -> Lude.Maybe Lude.Text) (\s a -> s {test = a} :: CreateQualificationType)
{-# DEPRECATED cqtTest "Use generic-lens or generic-optics with 'test' instead." #-}

-- | The name you give to the Qualification type. The type name is used to represent the Qualification to Workers, and to find the type using a Qualification type search. It must be unique across all of your Qualification types.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqtName :: Lens.Lens' CreateQualificationType Lude.Text
cqtName = Lens.lens (name :: CreateQualificationType -> Lude.Text) (\s a -> s {name = a} :: CreateQualificationType)
{-# DEPRECATED cqtName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | One or more words or phrases that describe the Qualification type, separated by commas. The keywords of a type make the type easier to find during a search.
--
-- /Note:/ Consider using 'keywords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqtKeywords :: Lens.Lens' CreateQualificationType (Lude.Maybe Lude.Text)
cqtKeywords = Lens.lens (keywords :: CreateQualificationType -> Lude.Maybe Lude.Text) (\s a -> s {keywords = a} :: CreateQualificationType)
{-# DEPRECATED cqtKeywords "Use generic-lens or generic-optics with 'keywords' instead." #-}

-- | Specifies whether requests for the Qualification type are granted immediately, without prompting the Worker with a Qualification test.
--
-- Constraints: If the Test parameter is specified, this parameter cannot be true.
--
-- /Note:/ Consider using 'autoGranted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqtAutoGranted :: Lens.Lens' CreateQualificationType (Lude.Maybe Lude.Bool)
cqtAutoGranted = Lens.lens (autoGranted :: CreateQualificationType -> Lude.Maybe Lude.Bool) (\s a -> s {autoGranted = a} :: CreateQualificationType)
{-# DEPRECATED cqtAutoGranted "Use generic-lens or generic-optics with 'autoGranted' instead." #-}

-- | The Qualification value to use for automatically granted Qualifications. This parameter is used only if the AutoGranted parameter is true.
--
-- /Note:/ Consider using 'autoGrantedValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqtAutoGrantedValue :: Lens.Lens' CreateQualificationType (Lude.Maybe Lude.Int)
cqtAutoGrantedValue = Lens.lens (autoGrantedValue :: CreateQualificationType -> Lude.Maybe Lude.Int) (\s a -> s {autoGrantedValue = a} :: CreateQualificationType)
{-# DEPRECATED cqtAutoGrantedValue "Use generic-lens or generic-optics with 'autoGrantedValue' instead." #-}

-- | A long description for the Qualification type. On the Amazon Mechanical Turk website, the long description is displayed when a Worker examines a Qualification type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqtDescription :: Lens.Lens' CreateQualificationType Lude.Text
cqtDescription = Lens.lens (description :: CreateQualificationType -> Lude.Text) (\s a -> s {description = a} :: CreateQualificationType)
{-# DEPRECATED cqtDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The number of seconds that a Worker must wait after requesting a Qualification of the Qualification type before the worker can retry the Qualification request.
--
-- Constraints: None. If not specified, retries are disabled and Workers can request a Qualification of this type only once, even if the Worker has not been granted the Qualification. It is not possible to disable retries for a Qualification type after it has been created with retries enabled. If you want to disable retries, you must delete existing retry-enabled Qualification type and then create a new Qualification type with retries disabled.
--
-- /Note:/ Consider using 'retryDelayInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqtRetryDelayInSeconds :: Lens.Lens' CreateQualificationType (Lude.Maybe Lude.Integer)
cqtRetryDelayInSeconds = Lens.lens (retryDelayInSeconds :: CreateQualificationType -> Lude.Maybe Lude.Integer) (\s a -> s {retryDelayInSeconds = a} :: CreateQualificationType)
{-# DEPRECATED cqtRetryDelayInSeconds "Use generic-lens or generic-optics with 'retryDelayInSeconds' instead." #-}

instance Lude.AWSRequest CreateQualificationType where
  type Rs CreateQualificationType = CreateQualificationTypeResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateQualificationTypeResponse'
            Lude.<$> (x Lude..?> "QualificationType")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateQualificationType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.CreateQualificationType" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateQualificationType where
  toJSON CreateQualificationType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TestDurationInSeconds" Lude..=) Lude.<$> testDurationInSeconds,
            Lude.Just
              ("QualificationTypeStatus" Lude..= qualificationTypeStatus),
            ("AnswerKey" Lude..=) Lude.<$> answerKey,
            ("Test" Lude..=) Lude.<$> test,
            Lude.Just ("Name" Lude..= name),
            ("Keywords" Lude..=) Lude.<$> keywords,
            ("AutoGranted" Lude..=) Lude.<$> autoGranted,
            ("AutoGrantedValue" Lude..=) Lude.<$> autoGrantedValue,
            Lude.Just ("Description" Lude..= description),
            ("RetryDelayInSeconds" Lude..=) Lude.<$> retryDelayInSeconds
          ]
      )

instance Lude.ToPath CreateQualificationType where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateQualificationType where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateQualificationTypeResponse' smart constructor.
data CreateQualificationTypeResponse = CreateQualificationTypeResponse'
  { -- | The created Qualification type, returned as a QualificationType data structure.
    qualificationType :: Lude.Maybe QualificationType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateQualificationTypeResponse' with the minimum fields required to make a request.
--
-- * 'qualificationType' - The created Qualification type, returned as a QualificationType data structure.
-- * 'responseStatus' - The response status code.
mkCreateQualificationTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateQualificationTypeResponse
mkCreateQualificationTypeResponse pResponseStatus_ =
  CreateQualificationTypeResponse'
    { qualificationType =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The created Qualification type, returned as a QualificationType data structure.
--
-- /Note:/ Consider using 'qualificationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqtrsQualificationType :: Lens.Lens' CreateQualificationTypeResponse (Lude.Maybe QualificationType)
cqtrsQualificationType = Lens.lens (qualificationType :: CreateQualificationTypeResponse -> Lude.Maybe QualificationType) (\s a -> s {qualificationType = a} :: CreateQualificationTypeResponse)
{-# DEPRECATED cqtrsQualificationType "Use generic-lens or generic-optics with 'qualificationType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqtrsResponseStatus :: Lens.Lens' CreateQualificationTypeResponse Lude.Int
cqtrsResponseStatus = Lens.lens (responseStatus :: CreateQualificationTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateQualificationTypeResponse)
{-# DEPRECATED cqtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
