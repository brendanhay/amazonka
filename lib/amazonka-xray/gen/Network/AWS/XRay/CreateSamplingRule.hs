{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.CreateSamplingRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a rule to control sampling behavior for instrumented applications. Services retrieve rules with 'GetSamplingRules' , and evaluate each rule in ascending order of /priority/ for each request. If a rule matches, the service records a trace, borrowing it from the reservoir size. After 10 seconds, the service reports back to X-Ray with 'GetSamplingTargets' to get updated versions of each in-use rule. The updated rule contains a trace quota that the service can use instead of borrowing from the reservoir.
module Network.AWS.XRay.CreateSamplingRule
  ( -- * Creating a request
    CreateSamplingRule (..),
    mkCreateSamplingRule,

    -- ** Request lenses
    csrSamplingRule,
    csrTags,

    -- * Destructuring the response
    CreateSamplingRuleResponse (..),
    mkCreateSamplingRuleResponse,

    -- ** Response lenses
    csrrsSamplingRuleRecord,
    csrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.XRay.Types

-- | /See:/ 'mkCreateSamplingRule' smart constructor.
data CreateSamplingRule = CreateSamplingRule'
  { -- | The rule definition.
    samplingRule :: SamplingRule,
    -- | A map that contains one or more tag keys and tag values to attach to an X-Ray sampling rule. For more information about ways to use tags, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources> in the /AWS General Reference/ .
    --
    -- The following restrictions apply to tags:
    --
    --     * Maximum number of user-applied tags per resource: 50
    --
    --
    --     * Maximum tag key length: 128 Unicode characters
    --
    --
    --     * Maximum tag value length: 256 Unicode characters
    --
    --
    --     * Valid values for key and value: a-z, A-Z, 0-9, space, and the following characters: _ . : / = + - and @
    --
    --
    --     * Tag keys and values are case sensitive.
    --
    --
    --     * Don't use @aws:@ as a prefix for keys; it's reserved for AWS use.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSamplingRule' with the minimum fields required to make a request.
--
-- * 'samplingRule' - The rule definition.
-- * 'tags' - A map that contains one or more tag keys and tag values to attach to an X-Ray sampling rule. For more information about ways to use tags, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources> in the /AWS General Reference/ .
--
-- The following restrictions apply to tags:
--
--     * Maximum number of user-applied tags per resource: 50
--
--
--     * Maximum tag key length: 128 Unicode characters
--
--
--     * Maximum tag value length: 256 Unicode characters
--
--
--     * Valid values for key and value: a-z, A-Z, 0-9, space, and the following characters: _ . : / = + - and @
--
--
--     * Tag keys and values are case sensitive.
--
--
--     * Don't use @aws:@ as a prefix for keys; it's reserved for AWS use.
mkCreateSamplingRule ::
  -- | 'samplingRule'
  SamplingRule ->
  CreateSamplingRule
mkCreateSamplingRule pSamplingRule_ =
  CreateSamplingRule'
    { samplingRule = pSamplingRule_,
      tags = Lude.Nothing
    }

-- | The rule definition.
--
-- /Note:/ Consider using 'samplingRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrSamplingRule :: Lens.Lens' CreateSamplingRule SamplingRule
csrSamplingRule = Lens.lens (samplingRule :: CreateSamplingRule -> SamplingRule) (\s a -> s {samplingRule = a} :: CreateSamplingRule)
{-# DEPRECATED csrSamplingRule "Use generic-lens or generic-optics with 'samplingRule' instead." #-}

-- | A map that contains one or more tag keys and tag values to attach to an X-Ray sampling rule. For more information about ways to use tags, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources> in the /AWS General Reference/ .
--
-- The following restrictions apply to tags:
--
--     * Maximum number of user-applied tags per resource: 50
--
--
--     * Maximum tag key length: 128 Unicode characters
--
--
--     * Maximum tag value length: 256 Unicode characters
--
--
--     * Valid values for key and value: a-z, A-Z, 0-9, space, and the following characters: _ . : / = + - and @
--
--
--     * Tag keys and values are case sensitive.
--
--
--     * Don't use @aws:@ as a prefix for keys; it's reserved for AWS use.
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrTags :: Lens.Lens' CreateSamplingRule (Lude.Maybe [Tag])
csrTags = Lens.lens (tags :: CreateSamplingRule -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateSamplingRule)
{-# DEPRECATED csrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateSamplingRule where
  type Rs CreateSamplingRule = CreateSamplingRuleResponse
  request = Req.postJSON xRayService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateSamplingRuleResponse'
            Lude.<$> (x Lude..?> "SamplingRuleRecord")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSamplingRule where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateSamplingRule where
  toJSON CreateSamplingRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SamplingRule" Lude..= samplingRule),
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateSamplingRule where
  toPath = Lude.const "/CreateSamplingRule"

instance Lude.ToQuery CreateSamplingRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateSamplingRuleResponse' smart constructor.
data CreateSamplingRuleResponse = CreateSamplingRuleResponse'
  { -- | The saved rule definition and metadata.
    samplingRuleRecord :: Lude.Maybe SamplingRuleRecord,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSamplingRuleResponse' with the minimum fields required to make a request.
--
-- * 'samplingRuleRecord' - The saved rule definition and metadata.
-- * 'responseStatus' - The response status code.
mkCreateSamplingRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSamplingRuleResponse
mkCreateSamplingRuleResponse pResponseStatus_ =
  CreateSamplingRuleResponse'
    { samplingRuleRecord = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The saved rule definition and metadata.
--
-- /Note:/ Consider using 'samplingRuleRecord' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsSamplingRuleRecord :: Lens.Lens' CreateSamplingRuleResponse (Lude.Maybe SamplingRuleRecord)
csrrsSamplingRuleRecord = Lens.lens (samplingRuleRecord :: CreateSamplingRuleResponse -> Lude.Maybe SamplingRuleRecord) (\s a -> s {samplingRuleRecord = a} :: CreateSamplingRuleResponse)
{-# DEPRECATED csrrsSamplingRuleRecord "Use generic-lens or generic-optics with 'samplingRuleRecord' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CreateSamplingRuleResponse Lude.Int
csrrsResponseStatus = Lens.lens (responseStatus :: CreateSamplingRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSamplingRuleResponse)
{-# DEPRECATED csrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
