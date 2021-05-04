{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudFront.Types.LambdaFunctionAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.LambdaFunctionAssociation where

import Network.AWS.CloudFront.Types.EventType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A complex type that contains a Lambda function association.
--
-- /See:/ 'newLambdaFunctionAssociation' smart constructor.
data LambdaFunctionAssociation = LambdaFunctionAssociation'
  { -- | A flag that allows a Lambda function to have read access to the body
    -- content. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/lambda-include-body-access.html Accessing the Request Body by Choosing the Include Body Option>
    -- in the Amazon CloudFront Developer Guide.
    includeBody :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the Lambda function. You must specify the ARN of a function
    -- version; you can\'t specify a Lambda alias or $LATEST.
    lambdaFunctionARN :: Prelude.Text,
    -- | Specifies the event type that triggers a Lambda function invocation. You
    -- can specify the following values:
    --
    -- -   @viewer-request@: The function executes when CloudFront receives a
    --     request from a viewer and before it checks to see whether the
    --     requested object is in the edge cache.
    --
    -- -   @origin-request@: The function executes only when CloudFront sends a
    --     request to your origin. When the requested object is in the edge
    --     cache, the function doesn\'t execute.
    --
    -- -   @origin-response@: The function executes after CloudFront receives a
    --     response from the origin and before it caches the object in the
    --     response. When the requested object is in the edge cache, the
    --     function doesn\'t execute.
    --
    -- -   @viewer-response@: The function executes before CloudFront returns
    --     the requested object to the viewer. The function executes regardless
    --     of whether the object was already in the edge cache.
    --
    --     If the origin returns an HTTP status code other than HTTP 200 (OK),
    --     the function doesn\'t execute.
    eventType :: EventType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeBody', 'lambdaFunctionAssociation_includeBody' - A flag that allows a Lambda function to have read access to the body
-- content. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/lambda-include-body-access.html Accessing the Request Body by Choosing the Include Body Option>
-- in the Amazon CloudFront Developer Guide.
--
-- 'lambdaFunctionARN', 'lambdaFunctionAssociation_lambdaFunctionARN' - The ARN of the Lambda function. You must specify the ARN of a function
-- version; you can\'t specify a Lambda alias or $LATEST.
--
-- 'eventType', 'lambdaFunctionAssociation_eventType' - Specifies the event type that triggers a Lambda function invocation. You
-- can specify the following values:
--
-- -   @viewer-request@: The function executes when CloudFront receives a
--     request from a viewer and before it checks to see whether the
--     requested object is in the edge cache.
--
-- -   @origin-request@: The function executes only when CloudFront sends a
--     request to your origin. When the requested object is in the edge
--     cache, the function doesn\'t execute.
--
-- -   @origin-response@: The function executes after CloudFront receives a
--     response from the origin and before it caches the object in the
--     response. When the requested object is in the edge cache, the
--     function doesn\'t execute.
--
-- -   @viewer-response@: The function executes before CloudFront returns
--     the requested object to the viewer. The function executes regardless
--     of whether the object was already in the edge cache.
--
--     If the origin returns an HTTP status code other than HTTP 200 (OK),
--     the function doesn\'t execute.
newLambdaFunctionAssociation ::
  -- | 'lambdaFunctionARN'
  Prelude.Text ->
  -- | 'eventType'
  EventType ->
  LambdaFunctionAssociation
newLambdaFunctionAssociation
  pLambdaFunctionARN_
  pEventType_ =
    LambdaFunctionAssociation'
      { includeBody =
          Prelude.Nothing,
        lambdaFunctionARN = pLambdaFunctionARN_,
        eventType = pEventType_
      }

-- | A flag that allows a Lambda function to have read access to the body
-- content. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/lambda-include-body-access.html Accessing the Request Body by Choosing the Include Body Option>
-- in the Amazon CloudFront Developer Guide.
lambdaFunctionAssociation_includeBody :: Lens.Lens' LambdaFunctionAssociation (Prelude.Maybe Prelude.Bool)
lambdaFunctionAssociation_includeBody = Lens.lens (\LambdaFunctionAssociation' {includeBody} -> includeBody) (\s@LambdaFunctionAssociation' {} a -> s {includeBody = a} :: LambdaFunctionAssociation)

-- | The ARN of the Lambda function. You must specify the ARN of a function
-- version; you can\'t specify a Lambda alias or $LATEST.
lambdaFunctionAssociation_lambdaFunctionARN :: Lens.Lens' LambdaFunctionAssociation Prelude.Text
lambdaFunctionAssociation_lambdaFunctionARN = Lens.lens (\LambdaFunctionAssociation' {lambdaFunctionARN} -> lambdaFunctionARN) (\s@LambdaFunctionAssociation' {} a -> s {lambdaFunctionARN = a} :: LambdaFunctionAssociation)

-- | Specifies the event type that triggers a Lambda function invocation. You
-- can specify the following values:
--
-- -   @viewer-request@: The function executes when CloudFront receives a
--     request from a viewer and before it checks to see whether the
--     requested object is in the edge cache.
--
-- -   @origin-request@: The function executes only when CloudFront sends a
--     request to your origin. When the requested object is in the edge
--     cache, the function doesn\'t execute.
--
-- -   @origin-response@: The function executes after CloudFront receives a
--     response from the origin and before it caches the object in the
--     response. When the requested object is in the edge cache, the
--     function doesn\'t execute.
--
-- -   @viewer-response@: The function executes before CloudFront returns
--     the requested object to the viewer. The function executes regardless
--     of whether the object was already in the edge cache.
--
--     If the origin returns an HTTP status code other than HTTP 200 (OK),
--     the function doesn\'t execute.
lambdaFunctionAssociation_eventType :: Lens.Lens' LambdaFunctionAssociation EventType
lambdaFunctionAssociation_eventType = Lens.lens (\LambdaFunctionAssociation' {eventType} -> eventType) (\s@LambdaFunctionAssociation' {} a -> s {eventType = a} :: LambdaFunctionAssociation)

instance Prelude.FromXML LambdaFunctionAssociation where
  parseXML x =
    LambdaFunctionAssociation'
      Prelude.<$> (x Prelude..@? "IncludeBody")
      Prelude.<*> (x Prelude..@ "LambdaFunctionARN")
      Prelude.<*> (x Prelude..@ "EventType")

instance Prelude.Hashable LambdaFunctionAssociation

instance Prelude.NFData LambdaFunctionAssociation

instance Prelude.ToXML LambdaFunctionAssociation where
  toXML LambdaFunctionAssociation' {..} =
    Prelude.mconcat
      [ "IncludeBody" Prelude.@= includeBody,
        "LambdaFunctionARN" Prelude.@= lambdaFunctionARN,
        "EventType" Prelude.@= eventType
      ]
