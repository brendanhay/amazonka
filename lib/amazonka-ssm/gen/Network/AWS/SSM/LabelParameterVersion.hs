{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.LabelParameterVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A parameter label is a user-defined alias to help you manage different versions of a parameter. When you modify a parameter, Systems Manager automatically saves a new version and increments the version number by one. A label can help you remember the purpose of a parameter when there are multiple versions.
--
-- Parameter labels have the following requirements and restrictions.
--
--     * A version of a parameter can have a maximum of 10 labels.
--
--
--     * You can't attach the same label to different versions of the same parameter. For example, if version 1 has the label Production, then you can't attach Production to version 2.
--
--
--     * You can move a label from one version of a parameter to another.
--
--
--     * You can't create a label when you create a new parameter. You must attach a label to a specific version of a parameter.
--
--
--     * You can't delete a parameter label. If you no longer want to use a parameter label, then you must move it to a different version of a parameter.
--
--
--     * A label can have a maximum of 100 characters.
--
--
--     * Labels can contain letters (case sensitive), numbers, periods (.), hyphens (-), or underscores (_).
--
--
--     * Labels can't begin with a number, "aws," or "ssm" (not case sensitive). If a label fails to meet these requirements, then the label is not associated with a parameter and the system displays it in the list of InvalidLabels.
module Network.AWS.SSM.LabelParameterVersion
  ( -- * Creating a request
    LabelParameterVersion (..),
    mkLabelParameterVersion,

    -- ** Request lenses
    lpvName,
    lpvLabels,
    lpvParameterVersion,

    -- * Destructuring the response
    LabelParameterVersionResponse (..),
    mkLabelParameterVersionResponse,

    -- ** Response lenses
    lpvrsInvalidLabels,
    lpvrsParameterVersion,
    lpvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkLabelParameterVersion' smart constructor.
data LabelParameterVersion = LabelParameterVersion'
  { -- | The parameter name on which you want to attach one or more labels.
    name :: Lude.Text,
    -- | One or more labels to attach to the specified parameter version.
    labels :: Lude.NonEmpty Lude.Text,
    -- | The specific version of the parameter on which you want to attach one or more labels. If no version is specified, the system attaches the label to the latest version.
    parameterVersion :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LabelParameterVersion' with the minimum fields required to make a request.
--
-- * 'name' - The parameter name on which you want to attach one or more labels.
-- * 'labels' - One or more labels to attach to the specified parameter version.
-- * 'parameterVersion' - The specific version of the parameter on which you want to attach one or more labels. If no version is specified, the system attaches the label to the latest version.
mkLabelParameterVersion ::
  -- | 'name'
  Lude.Text ->
  -- | 'labels'
  Lude.NonEmpty Lude.Text ->
  LabelParameterVersion
mkLabelParameterVersion pName_ pLabels_ =
  LabelParameterVersion'
    { name = pName_,
      labels = pLabels_,
      parameterVersion = Lude.Nothing
    }

-- | The parameter name on which you want to attach one or more labels.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvName :: Lens.Lens' LabelParameterVersion Lude.Text
lpvName = Lens.lens (name :: LabelParameterVersion -> Lude.Text) (\s a -> s {name = a} :: LabelParameterVersion)
{-# DEPRECATED lpvName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | One or more labels to attach to the specified parameter version.
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvLabels :: Lens.Lens' LabelParameterVersion (Lude.NonEmpty Lude.Text)
lpvLabels = Lens.lens (labels :: LabelParameterVersion -> Lude.NonEmpty Lude.Text) (\s a -> s {labels = a} :: LabelParameterVersion)
{-# DEPRECATED lpvLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

-- | The specific version of the parameter on which you want to attach one or more labels. If no version is specified, the system attaches the label to the latest version.
--
-- /Note:/ Consider using 'parameterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvParameterVersion :: Lens.Lens' LabelParameterVersion (Lude.Maybe Lude.Integer)
lpvParameterVersion = Lens.lens (parameterVersion :: LabelParameterVersion -> Lude.Maybe Lude.Integer) (\s a -> s {parameterVersion = a} :: LabelParameterVersion)
{-# DEPRECATED lpvParameterVersion "Use generic-lens or generic-optics with 'parameterVersion' instead." #-}

instance Lude.AWSRequest LabelParameterVersion where
  type Rs LabelParameterVersion = LabelParameterVersionResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          LabelParameterVersionResponse'
            Lude.<$> (x Lude..?> "InvalidLabels")
            Lude.<*> (x Lude..?> "ParameterVersion")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders LabelParameterVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.LabelParameterVersion" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON LabelParameterVersion where
  toJSON LabelParameterVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("Labels" Lude..= labels),
            ("ParameterVersion" Lude..=) Lude.<$> parameterVersion
          ]
      )

instance Lude.ToPath LabelParameterVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery LabelParameterVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkLabelParameterVersionResponse' smart constructor.
data LabelParameterVersionResponse = LabelParameterVersionResponse'
  { -- | The label does not meet the requirements. For information about parameter label requirements, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-labels.html Labeling parameters> in the /AWS Systems Manager User Guide/ .
    invalidLabels :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The version of the parameter that has been labeled.
    parameterVersion :: Lude.Maybe Lude.Integer,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LabelParameterVersionResponse' with the minimum fields required to make a request.
--
-- * 'invalidLabels' - The label does not meet the requirements. For information about parameter label requirements, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-labels.html Labeling parameters> in the /AWS Systems Manager User Guide/ .
-- * 'parameterVersion' - The version of the parameter that has been labeled.
-- * 'responseStatus' - The response status code.
mkLabelParameterVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  LabelParameterVersionResponse
mkLabelParameterVersionResponse pResponseStatus_ =
  LabelParameterVersionResponse'
    { invalidLabels = Lude.Nothing,
      parameterVersion = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The label does not meet the requirements. For information about parameter label requirements, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-labels.html Labeling parameters> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'invalidLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvrsInvalidLabels :: Lens.Lens' LabelParameterVersionResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
lpvrsInvalidLabels = Lens.lens (invalidLabels :: LabelParameterVersionResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {invalidLabels = a} :: LabelParameterVersionResponse)
{-# DEPRECATED lpvrsInvalidLabels "Use generic-lens or generic-optics with 'invalidLabels' instead." #-}

-- | The version of the parameter that has been labeled.
--
-- /Note:/ Consider using 'parameterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvrsParameterVersion :: Lens.Lens' LabelParameterVersionResponse (Lude.Maybe Lude.Integer)
lpvrsParameterVersion = Lens.lens (parameterVersion :: LabelParameterVersionResponse -> Lude.Maybe Lude.Integer) (\s a -> s {parameterVersion = a} :: LabelParameterVersionResponse)
{-# DEPRECATED lpvrsParameterVersion "Use generic-lens or generic-optics with 'parameterVersion' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvrsResponseStatus :: Lens.Lens' LabelParameterVersionResponse Lude.Int
lpvrsResponseStatus = Lens.lens (responseStatus :: LabelParameterVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: LabelParameterVersionResponse)
{-# DEPRECATED lpvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
