{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.CreateConstraint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a constraint.
--
--
module Network.AWS.ServiceCatalog.CreateConstraint
    (
    -- * Creating a Request
      createConstraint
    , CreateConstraint
    -- * Request Lenses
    , ccAcceptLanguage
    , ccDescription
    , ccPortfolioId
    , ccProductId
    , ccParameters
    , ccType
    , ccIdempotencyToken

    -- * Destructuring the Response
    , createConstraintResponse
    , CreateConstraintResponse
    -- * Response Lenses
    , ccrsStatus
    , ccrsConstraintDetail
    , ccrsConstraintParameters
    , ccrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'createConstraint' smart constructor.
data CreateConstraint = CreateConstraint'
  { _ccAcceptLanguage   :: !(Maybe Text)
  , _ccDescription      :: !(Maybe Text)
  , _ccPortfolioId      :: !Text
  , _ccProductId        :: !Text
  , _ccParameters       :: !Text
  , _ccType             :: !Text
  , _ccIdempotencyToken :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateConstraint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'ccDescription' - The description of the constraint.
--
-- * 'ccPortfolioId' - The portfolio identifier.
--
-- * 'ccProductId' - The product identifier.
--
-- * 'ccParameters' - The constraint parameters, in JSON format. The syntax depends on the constraint type as follows:     * LAUNCH    * Specify the @RoleArn@ property as follows: \"RoleArn\" : \"arn:aws:iam::123456789012:role/LaunchRole\"     * NOTIFICATION    * Specify the @NotificationArns@ property as follows: \"NotificationArns\" : [\"arn:aws:sns:us-east-1:123456789012:Topic\"]     * TEMPLATE    * Specify the @Rules@ property. For more information, see <http://docs.aws.amazon.com/servicecatalog/latest/adminguide/reference-template_constraint_rules.html Template Constraint Rules> .
--
-- * 'ccType' - The type of constraint.     * @LAUNCH@      * @NOTIFICATION@      * @TEMPLATE@
--
-- * 'ccIdempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
createConstraint
    :: Text -- ^ 'ccPortfolioId'
    -> Text -- ^ 'ccProductId'
    -> Text -- ^ 'ccParameters'
    -> Text -- ^ 'ccType'
    -> Text -- ^ 'ccIdempotencyToken'
    -> CreateConstraint
createConstraint pPortfolioId_ pProductId_ pParameters_ pType_ pIdempotencyToken_ =
  CreateConstraint'
    { _ccAcceptLanguage = Nothing
    , _ccDescription = Nothing
    , _ccPortfolioId = pPortfolioId_
    , _ccProductId = pProductId_
    , _ccParameters = pParameters_
    , _ccType = pType_
    , _ccIdempotencyToken = pIdempotencyToken_
    }


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
ccAcceptLanguage :: Lens' CreateConstraint (Maybe Text)
ccAcceptLanguage = lens _ccAcceptLanguage (\ s a -> s{_ccAcceptLanguage = a})

-- | The description of the constraint.
ccDescription :: Lens' CreateConstraint (Maybe Text)
ccDescription = lens _ccDescription (\ s a -> s{_ccDescription = a})

-- | The portfolio identifier.
ccPortfolioId :: Lens' CreateConstraint Text
ccPortfolioId = lens _ccPortfolioId (\ s a -> s{_ccPortfolioId = a})

-- | The product identifier.
ccProductId :: Lens' CreateConstraint Text
ccProductId = lens _ccProductId (\ s a -> s{_ccProductId = a})

-- | The constraint parameters, in JSON format. The syntax depends on the constraint type as follows:     * LAUNCH    * Specify the @RoleArn@ property as follows: \"RoleArn\" : \"arn:aws:iam::123456789012:role/LaunchRole\"     * NOTIFICATION    * Specify the @NotificationArns@ property as follows: \"NotificationArns\" : [\"arn:aws:sns:us-east-1:123456789012:Topic\"]     * TEMPLATE    * Specify the @Rules@ property. For more information, see <http://docs.aws.amazon.com/servicecatalog/latest/adminguide/reference-template_constraint_rules.html Template Constraint Rules> .
ccParameters :: Lens' CreateConstraint Text
ccParameters = lens _ccParameters (\ s a -> s{_ccParameters = a})

-- | The type of constraint.     * @LAUNCH@      * @NOTIFICATION@      * @TEMPLATE@
ccType :: Lens' CreateConstraint Text
ccType = lens _ccType (\ s a -> s{_ccType = a})

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
ccIdempotencyToken :: Lens' CreateConstraint Text
ccIdempotencyToken = lens _ccIdempotencyToken (\ s a -> s{_ccIdempotencyToken = a})

instance AWSRequest CreateConstraint where
        type Rs CreateConstraint = CreateConstraintResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 CreateConstraintResponse' <$>
                   (x .?> "Status") <*> (x .?> "ConstraintDetail") <*>
                     (x .?> "ConstraintParameters")
                     <*> (pure (fromEnum s)))

instance Hashable CreateConstraint where

instance NFData CreateConstraint where

instance ToHeaders CreateConstraint where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.CreateConstraint" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateConstraint where
        toJSON CreateConstraint'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _ccAcceptLanguage,
                  ("Description" .=) <$> _ccDescription,
                  Just ("PortfolioId" .= _ccPortfolioId),
                  Just ("ProductId" .= _ccProductId),
                  Just ("Parameters" .= _ccParameters),
                  Just ("Type" .= _ccType),
                  Just ("IdempotencyToken" .= _ccIdempotencyToken)])

instance ToPath CreateConstraint where
        toPath = const "/"

instance ToQuery CreateConstraint where
        toQuery = const mempty

-- | /See:/ 'createConstraintResponse' smart constructor.
data CreateConstraintResponse = CreateConstraintResponse'
  { _ccrsStatus               :: !(Maybe RequestStatus)
  , _ccrsConstraintDetail     :: !(Maybe ConstraintDetail)
  , _ccrsConstraintParameters :: !(Maybe Text)
  , _ccrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateConstraintResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsStatus' - The status of the current request.
--
-- * 'ccrsConstraintDetail' - Information about the constraint.
--
-- * 'ccrsConstraintParameters' - The constraint parameters.
--
-- * 'ccrsResponseStatus' - -- | The response status code.
createConstraintResponse
    :: Int -- ^ 'ccrsResponseStatus'
    -> CreateConstraintResponse
createConstraintResponse pResponseStatus_ =
  CreateConstraintResponse'
    { _ccrsStatus = Nothing
    , _ccrsConstraintDetail = Nothing
    , _ccrsConstraintParameters = Nothing
    , _ccrsResponseStatus = pResponseStatus_
    }


-- | The status of the current request.
ccrsStatus :: Lens' CreateConstraintResponse (Maybe RequestStatus)
ccrsStatus = lens _ccrsStatus (\ s a -> s{_ccrsStatus = a})

-- | Information about the constraint.
ccrsConstraintDetail :: Lens' CreateConstraintResponse (Maybe ConstraintDetail)
ccrsConstraintDetail = lens _ccrsConstraintDetail (\ s a -> s{_ccrsConstraintDetail = a})

-- | The constraint parameters.
ccrsConstraintParameters :: Lens' CreateConstraintResponse (Maybe Text)
ccrsConstraintParameters = lens _ccrsConstraintParameters (\ s a -> s{_ccrsConstraintParameters = a})

-- | -- | The response status code.
ccrsResponseStatus :: Lens' CreateConstraintResponse Int
ccrsResponseStatus = lens _ccrsResponseStatus (\ s a -> s{_ccrsResponseStatus = a})

instance NFData CreateConstraintResponse where
